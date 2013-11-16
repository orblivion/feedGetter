{-# LANGUAGE DeriveDataTypeable #-}

import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Conduit (http, withManager, parseUrl, simpleHttp, Request, responseBody)
import qualified Data.Conduit as C
import Network.URI
import Control.Concurrent.Async
import Text.XML.Light
import Text.XML.Light.Input
import Text.XML.Light.Proc
import Data.Yaml.YamlLight
import Text.Groom
import Data.Either
import Data.Maybe
import Data.Map as DM
import Data.List.Split
import Data.List
import Data.Typeable
import System.FilePath
import System.Directory
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSLazy
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.Monad.STM
import Control.Concurrent.STM.TChan
import System.Random.Shuffle
import Prelude as P
 
----
-- General Utilities
----

-- A commonly used type signature
type EitherTIO a = EitherT SomeException IO a

-- Wrap any operation that throws an exception to an EitherTIO
errToEitherT :: a -> EitherTIO a
errToEitherT = EitherT . try . return

-- Used in a couple different structs
type URL = String

----
-- XML Utilities
----

-- Certain XML library functions take QNames to refer to tag names, etc, which requires
-- that you call unqual :: String -> QName on every such parameter. unqualifyfy turns
-- such functions into functions that accept String
unqualifyfy :: (QName -> t) -> String -> t
unqualifyfy f = f' where
    f' a = f (unqual a)

-- String, instead of QName, versions of these XML library functions.
findElements' = unqualifyfy findElements
findChild' = unqualifyfy findChild
findAttr' = unqualifyfy findAttr

data SimpleXMLRepr = SElementRepr String [(String, String)] [SimpleXMLRepr] | STextRepr String | SDontCareRepr

-- Simplify the XML representation, it's more complicated than needed for our purposes
simpleXML' :: Element -> SimpleXMLRepr
simpleXML' e = simpleXML $ Elem e
simpleXML (Elem e) = SElementRepr name attrs content where
    name = (qName . elName) e
    attrs = P.map simpleAttr (elAttribs e)
    content = P.map simpleXML (elContent e)
    simpleAttr attr = (qName $ attrKey attr, attrVal attr)
simpleXML (Text e) = STextRepr $ cdData e
simpleXML _ = SDontCareRepr

prettyShowNodes :: SimpleXMLRepr -> [Char]
prettyShowNodes s = show' 0 s where
    indent ind = (take ind $ repeat ' ')
    show' ind (SElementRepr name attrs subelems) = indent ind ++ name ++ "\n" ++ attrDisp attrs ++ "\n" ++ subElemDisp where
        attrDisp [] = ""
        attrDisp attrs = concatMap showAttr attrs
        showAttr (a, b) = indent ind ++ "  " ++ a ++ "=" ++ "\"" ++ b ++ "\"" ++ "\n"
        subElemDisp = concatMap (show' (ind + 2)) subelems
    show' ind (STextRepr str) = indent ind ++ "\"" ++ str ++ "\"\n"
    show' _ SDontCareRepr = ""


-- From a list of high level nodes, we want to find all lower level nodes that are
-- found by walking down a path of tag names. This function takes a list of tag
-- names, and a list of higher level nodes, and returns the lower level nodes.
-- Example: 
-- downXMLPath ["rss", "channel", "item"] elements
-- if some of the given elements are <rss> tags, and some of those contain (possibly
-- multiple) <channel> tags, and some of those contain (possibly multiple) <item> tags,
-- this function will return all of the <item> tags.
downXMLPath :: [String] -> [Element] -> [Element]
downXMLPath [] = id
downXMLPath (tag_name:next_names) = (downXMLPath next_names) . (downXMLPath' tag_name)

downXMLPath' [] elems = elems
downXMLPath' tag_name elems = concatMap (findElements' tag_name) $ elems


----
-- Feed Specification
----

-- The structure of the feeds I specify that I want
type FileInfoGetter = (Element -> Maybe String, Element -> Maybe String)
data FeedSpec = FeedSpec { 
    feedName :: String, 
    rssFeedURL :: URL,
    feedRelPath :: Maybe FilePath,
    itemNodeToFileInfo :: Maybe FileInfoGetter,
    maxEntriesToGet :: Maybe Int
    }

data ShowableFeedSpec = ShowableFeedSpec {
    feedName' :: String, 
    rssFeedURL' :: URL,
    feedRelPath' :: Maybe FilePath,
    maxEntriesToGet' :: Maybe Int
    } deriving Show

instance Show FeedSpec where
    show feedSpec = groom $ ShowableFeedSpec {
        feedName' = (feedName feedSpec),
        rssFeedURL' = (rssFeedURL feedSpec),
        feedRelPath' = (feedRelPath feedSpec),
        maxEntriesToGet' = (maxEntriesToGet feedSpec)
    }

-- Reading Yaml configuration

data YamlException = YamlException String
    deriving (Show, Typeable)

instance Exception YamlException
yamlError str = hoistEither $ Left $ SomeException $ YamlException str

readFeedConfig :: FilePath -> EitherTIO [FeedSpec]
readFeedConfig filePath = do
    allSpecsYaml <- (EitherT . try) $ parseYamlFile filePath
    entries <- yamlToEntries allSpecsYaml
    mapM entryToFeedSpec entries
        where 
            yLookup :: [Char] -> (Map YamlLight YamlLight) -> Maybe [Char]
            yLookup key map = do
                yaml <- DM.lookup (YStr $ BS8.pack key) map
                case yaml of
                    YStr str -> return $ BS8.unpack str
                    _ -> Nothing
                
            yamlToEntries :: YamlLight -> EitherTIO [(YamlLight, YamlLight)]
            yamlToEntries (YMap entryMap) = return $ toList entryMap where
            yamlToEntries _ = yamlError "Badly formatted feed specification file."
            entryToFeedSpec :: (YamlLight, YamlLight) -> EitherTIO FeedSpec
            entryToFeedSpec ((YStr feedName), (YMap fileMap)) = do
                -- either, not eithert. hmm.
                url <- errToEitherT . fromJust $ yLookup "url" fileMap
                let fileInfoGetterName = yLookup "itemNodeToFileInfo" fileMap
                let maxEntriesToGetStr = yLookup "maxEntriesToGet" fileMap
                feedRelPath <- sanitizeFeedRelPath $ yLookup "feedRelPath" fileMap
                maxEntriesToGet <- errToEitherT $ maxEntriesToGetStr >>= Just . read

                return FeedSpec {
                  feedName = BS8.unpack feedName,
                  rssFeedURL = url,
                  feedRelPath = feedRelPath,
                  itemNodeToFileInfo = fileInfoGetterName >>= getFileInfoGetter,
                  maxEntriesToGet = maxEntriesToGet 
                }
            entryToFeedSpec _ = yamlError $ "Badly formatted yaml entry. Are you missing a field?"

----
-- RSS
----

-- Some RSS-format based specifics

from_enclosure :: FileInfoGetter
from_enclosure = (getFilePath, getExtension) where
    getFilePath item = (findChild' "enclosure" item) >>= findAttr' "url"
    getExtension item = (findChild' "enclosure" item) >>= findAttr' "type" >>= findExtension where
        findExtension "audio/mpeg" = Just "mp3"
        findExtension "audio/ogg" = Just "ogg"
        findExtension _ = Nothing

getFileInfoGetter :: String -> Maybe FileInfoGetter
getFileInfoGetter name = P.lookup name [("from_enclosure", from_enclosure)]

defaultingItemNodeToFileInfo :: FeedSpec -> FileInfoGetter
defaultingItemNodeToFileInfo = (fromMaybe from_enclosure) . itemNodeToFileInfo 
itemNodeToUrl = fst . defaultingItemNodeToFileInfo
itemNodeToExtension = snd . defaultingItemNodeToFileInfo

-- What comes out in reality, based on my specifications

data RSSFeed = RSSFeed {rssFeedSpec :: FeedSpec, rssFeedEntries :: [RSSEntry], xmlContent :: [Content]}
data RSSEntry = RSSEntry {
    rssEntryFeedSpec :: FeedSpec, rssEntryTitle :: Maybe String,
    rssEntryURL :: URL, rssEntryElement :: Element
} deriving Show
type FeedSpecError = (FeedSpec, SomeException)
type RSSEntryError = (RSSEntry, SomeException)
data ContentFileJob' m = ContentFileJob' {
    contentFileJobRSSEntry :: RSSEntry,
    contentFileJobRequest  :: (Request m),
    contentFileJobFilePath :: FilePath
    }
type ContentFileJob = ContentFileJob' (C.ResourceT IO)

-- Given an RSS Feed, grab the latest entries
getLatestEntries :: RSSFeed -> [RSSEntry]
getLatestEntries rssFeed = take (fromMaybe 5 $ maxEntriesToGet $ rssFeedSpec rssFeed) $ rssFeedEntries rssFeed

-- Starting from the top level elements (representations of XML data), return a
-- list of the lower level elements within, which each represent a single rss
-- entry
getItemNodes :: [Content] -> [Element]
getItemNodes top_elements = downXMLPath ["rss", "channel", "item"] (onlyElems top_elements)

-- Given a representation of XML nodes, return a list of RSS entry structures
getRSSEntries :: [Content] -> FeedSpec -> [RSSEntry]
getRSSEntries top_elements rssSpec = entries where
    items = concatMap (filterElements (isJust . getURL)) $ getItemNodes top_elements
    getURL item = itemNodeToUrl rssSpec item

    entries = [
            RSSEntry {
            rssEntryTitle=fmap strContent $ findChild' "title" item,
            rssEntryURL=fromJust $ getURL item,
            rssEntryFeedSpec=rssSpec,
            rssEntryElement=item
        } 
        | item <- items ]

-- Given a FeedSpec, download the feed file.
getRSSFeed :: FeedSpec -> IO RSSFeed
getRSSFeed rssSpec = do
    putStr $ "Downloading Feed: " ++ (rssFeedURL rssSpec) ++ "\n"
    feedData <- simpleHttp $ rssFeedURL rssSpec
    let content = (parseXML feedData)
    return $ RSSFeed rssSpec (getRSSEntries content rssSpec) content

----
-- Content File Getting
----

-- Given an RSSEntry, and a file name, create a ContentFileJob structure
getContentFileJob :: (RSSEntry, String) -> IO (Either RSSEntryError ContentFileJob) 
getContentFileJob (rssEntry, fileName) = do
    eitherRequest <- try $ parseUrl $ rssEntryURL rssEntry
    case eitherRequest of
        Left exception -> return $ Left (rssEntry, exception)
        Right request -> return $ Right $ ContentFileJob' {
            contentFileJobRSSEntry = rssEntry,
            contentFileJobRequest = request,
            contentFileJobFilePath = fileName
        }

-- ContentFileJob has the URL to grab from and the file path to save to. This
-- function does the job. It creates a temporary file until the download is
-- complete, and it skips any files that are already there.
runContentFileJob :: ContentFileJob -> IO ()
runContentFileJob contentFileJob = do
    let finalContentFilePath = contentFileJobFilePath contentFileJob
    let tmpContentFilePath = finalContentFilePath ++ "~"
    alreadyHave <- doesFileExist finalContentFilePath
    case alreadyHave of
        False -> do
            putStr $ "Downloading: " ++ finalContentFilePath ++ "\n"
            createDirectoryIfMissing True $ takeDirectory finalContentFilePath
            download (contentFileJobRequest contentFileJob) tmpContentFilePath
            renameFile tmpContentFilePath finalContentFilePath
            return ()
        True -> do
            putStr $ "Already Have: " ++ finalContentFilePath ++ "\n"
        where
            download request path = do
                withManager $ \manager -> do
                    response <- http request manager
                    responseBody response C.$$+- sinkFile path

alphaNumeric = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

-- Given a potentially dirty name (probably taken from a URL), replace any
-- characters that are not alphanumeric, . or - with _. 
sanitizeForFileName :: String -> String
sanitizeForFileName "" = "item"
sanitizeForFileName raw_file_name = P.map sanitizeChar raw_file_name where
    sanitizeChar char
        | not $ elem char (alphaNumeric ++ ".-") = '_'
        | otherwise = char

-- Given a relative path for the feed to go into, create a cleaned up version,
-- and throw an error for any characters not alphanumeric, _ or /. Somewhat
-- restrictive
sanitizeFeedRelPath :: Maybe String -> EitherTIO (Maybe String) 
sanitizeFeedRelPath Nothing = return Nothing
sanitizeFeedRelPath (Just str) = validate $ stripRev $ stripRev $ str where
    strip ('/':str) = strip str
    strip str = str
    stripRev :: String -> String
    stripRev = strip . reverse
    validate str
        | length str == 0 = yamlError "feedRelPath should have a directory name"
        | not $ all (flip elem (alphaNumeric ++ "_/")) str = yamlError "feedRelPath can only be alphanumerics and slashes" 
        | otherwise = return $ Just str

-- Given an RSS Entry, return a file name based purely on the URL of the entry
getContentFileName :: RSSEntry -> String
getContentFileName rssEntry = (sanitizeForFileName . normalize_extension) raw_file_name where
    -- let this error for now if a valid name can't be created (specifically expecting this for 
    -- extensions)
    raw_file_name = (last . (splitOn "/") . uriPath . fromJust . parseURI . rssEntryURL) rssEntry
    normalize_extension file_name
        | hasExtension file_name = file_name
        | otherwise = addExtension file_name extension
            where
                elementToExtension = (itemNodeToExtension . rssEntryFeedSpec $ rssEntry)
                -- fromJust will fail if there's no extension in the file name, nor specified any
                -- other way in the feed
                extension = (fromJust . elementToExtension . rssEntryElement) rssEntry

-- Given feeds root path, feed-specific relative file paths, and the file name,
-- get a full path for an rssEntry
getContentFilePath :: RSSEntry -> FilePath
getContentFilePath rssEntry = path where
    path = P.foldl combine "/" [
        rootPath,
        addRootDir contentFileExtraDir,
        getContentFileName rssEntry]
    fileDir = (sanitizeForFileName . feedName . rssEntryFeedSpec) rssEntry
    contentFileExtraDir = (feedRelPath . rssEntryFeedSpec) rssEntry
    addRootDir Nothing = fileDir
    addRootDir (Just extraDir) = combine extraDir fileDir

-- Given a list of file names with potential collisions, return the list with
-- unique names generated for any path collisions
getUniqueFileNames' :: [FilePath] -> [FilePath]
getUniqueFileNames' inNames = P.foldl uniquify [] $ reverse inNames where
    uniquify :: [FilePath] -> FilePath -> [FilePath]
    uniquify names_so_far name = uniqueName:names_so_far where
        uniqueName
            | elem name names_so_far = (
                replaceBaseName name 
                $ (takeBaseName name) 
                    ++ (show $ length names_so_far)
            )
            | otherwise = name

getUniqueFileNames :: [RSSEntry] -> [FilePath]
getUniqueFileNames = getUniqueFileNames' . (P.map getContentFilePath)

rootPath = "/home/haskell/feeds"

----
-- Processes
----

maxDownloadThreads = 5

-- Call a function f on everything coming out of fromChan and send the
-- result to toChan
relayTChan :: TChan a -> TChan b -> (a -> IO b) -> IO ()
relayTChan fromChan toChan f = do
    maybeVal <- atomically $ tryReadTChan fromChan
    case maybeVal of
        Just value -> do
            result <- f value
            atomically $ writeTChan toChan result
            relayTChan fromChan toChan f
        Nothing -> return ()

-- In the end we may want the results of these channeled operations for something.
-- Collect results as the chan operations finish.
collectTChan :: TChan a -> IO [a]
collectTChan chan = collectTChan' chan [] where
    collectTChan' chan accum = do
        maybeVal <- atomically $ tryReadTChan chan
        case maybeVal of
            Just value -> do
                collectTChan' chan (value:accum)
            Nothing -> return $ reverse accum


-- process_rss_feed_downloads is a single rss-file-getting thread. It grabs
-- a feed spec from the job channel (or quits if none are left), tries to
-- get the feed file, puts the result into the result channel, and starts over. An
-- arbitrary number of these threads can be spawned operating on the same channels
process_rss_feed_downloads :: TChan FeedSpec -> TChan ( Either FeedSpecError RSSFeed) -> IO ()
process_rss_feed_downloads jobChan resultChan = relayTChan jobChan resultChan run where
    run :: FeedSpec -> IO (Either FeedSpecError RSSFeed)
    run feedSpec = do
        result <- try $ getRSSFeed feedSpec
        case result of
            Left exception -> return $ Left (feedSpec, exception)
            Right rssFeed -> return $ Right rssFeed

-- process_content_file_jobs is a single content-file-getting thread. It grabs
-- a content file job from the job channel (or quits if none are left), tries to
-- get it, puts the result into the result channel, and starts over. An arbitrary
-- number of these threads can be spawned operating on the same channels
process_content_file_jobs :: TChan ContentFileJob -> TChan ( Either RSSEntryError RSSEntry) -> IO ()
process_content_file_jobs jobChan resultChan = relayTChan jobChan resultChan run where
    run :: ContentFileJob -> IO (Either RSSEntryError RSSEntry)
    run job = do
        result <- try $ runContentFileJob job
        let entry = contentFileJobRSSEntry job
        case result of
            Left exception -> return $ Left (entry, exception)
            Right _ -> return $ Right entry

-- Get the Feed Files (rss, atom)
get_feeds :: [FeedSpec] -> IO ([RSSFeed], [FeedSpecError], [RSSEntry])
get_feeds feedSpecs = do
    jobChan <- atomically $ newTChan
    resultChan <- atomically $ newTChan
    mapM (atomically . (writeTChan jobChan)) $ feedSpecs

    rssThreads <- mapM async $ replicate maxDownloadThreads $ process_rss_feed_downloads jobChan resultChan
    mapM waitCatch rssThreads
    rssFeedResults <- collectTChan resultChan

    let successRSSFeeds = rights rssFeedResults
    let errorFeedSpecs = lefts rssFeedResults
    let entries = successRSSFeeds >>= getLatestEntries

    return (successRSSFeeds, errorFeedSpecs, entries)

-- Get the Content Files (mp3, ogg)
get_content_files :: [RSSEntry] -> IO ([RSSEntry], [RSSEntryError])
get_content_files orderedEntries = do 
    entries <- shuffleM orderedEntries
    let entriesFilenames = getUniqueFileNames entries

    contentFileJobs <- mapM getContentFileJob $ zip entries entriesFilenames

    jobChan <- atomically $ newTChan
    resultChan <- atomically $ newTChan
    mapM (atomically . (writeTChan jobChan)) $ rights contentFileJobs

    contentThreads <- mapM async $ replicate maxDownloadThreads $ process_content_file_jobs jobChan resultChan
    mapM waitCatch contentThreads
    contentFileResults <- collectTChan resultChan

    let successRSSEntries = rights contentFileResults
    let errorRSSEntries = lefts contentFileResults ++ lefts contentFileJobs
    return (successRSSEntries, errorRSSEntries)

----
-- Debug Functions
----

-- Debug: Displays a representation of the RSS file, for ease of finding useful elements
debug_inspect_feed_file :: [RSSFeed] -> IO ()
debug_inspect_feed_file rssFeeds = do
    let allItemNodes = rssFeeds >>= getItemNodes . xmlContent >>= return . simpleXML'
    putStr "\n\n"
    putStr $ "Item Nodes: \n"
    putStr $ P.foldl (++) "" $ P.map prettyShowNodes allItemNodes
    putStr "\n\n"   

    return ()

-- Debug: Display the FeedSpecs derived from the yaml file
debug_yaml_reading :: [FeedSpec] -> IO ()
debug_yaml_reading feedSpecs = putStrLn $ groom feedSpecs


-- Debug: Display (Entry URL) for each entry
debug_entry_urls :: [RSSEntry] -> IO ()
debug_entry_urls entries = do
    putStr "\n\n"
    putStr $ "RSS Entries:\n" ++ groom ( P.map rssEntryURL entries )
    putStr "\n\n"

-- Debug: Display (Entry URL, Entry File Path) for each entry
debug_entry_urls_file_paths :: [RSSEntry] -> IO ()
debug_entry_urls_file_paths entries = do
    putStr "\n\n"
    putStr $ "All Entry URLs/Content File Paths, from entries: \n" ++ (
        groom $ zip (P.map rssEntryURL entries) (getUniqueFileNames entries))
    putStr "\n\n"


debug_entry_successes :: [RSSEntry] -> IO ()
debug_entry_successes successRSSEntries = do
    putStr "\n\n"
    putStr $ "RSS Content File Successes (Including skips from already having them):" ++ ( groom $ P.map rssEntryURL successRSSEntries )
    putStr "\n\n"

-- Debug: Show errors in downloading rss files.
debug_feed_file_errors :: [FeedSpecError] -> IO ()
debug_feed_file_errors errorFeedSpecs = do
    let showErr (feedSpec, error) = (feedName feedSpec, (take 100 $ show error) ++ "...")

    putStr "\n\n"
    putStr $ "RSS Feed File Errors:\n" ++ (groom $ P.map showErr errorFeedSpecs)
    putStr "\n\n"


debug_entry_errors :: [RSSEntryError] -> IO ()
debug_entry_errors errorRSSEntries = do
    let showErr (entry, error) = (rssEntryURL entry, (take 100 $ show error) ++ "...")

    putStr "\n\n"
    putStr $ "RSS Content File Errors:\n" ++ (groom $ P.map showErr errorRSSEntries)
    putStr "\n\n"


----
-- Main
----

main :: IO ()
main = do
    result <- runEitherT $ do
        feedSpecs <- readFeedConfig "feeds.yaml"
        lift $ do
            (successRSSFeeds, errorFeedSpecs, entries) <- get_feeds feedSpecs
            (successEntries, errorEntries) <- get_content_files entries

            -- uncomment as is useful for verbosity
            case True of
                True -> do -- enabled debug
                    debug_entry_errors errorEntries
                    debug_feed_file_errors errorFeedSpecs
                False -> do -- disabled debug, but still subject to type checking
                    debug_yaml_reading feedSpecs
                    debug_entry_urls entries
                    debug_entry_urls_file_paths entries
                    debug_inspect_feed_file successRSSFeeds
                    debug_entry_successes successEntries

            return ()
    case result of
        Left a -> putStrLn $ groom a
        _ -> return ()

    return ()
