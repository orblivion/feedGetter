{-# LANGUAGE DeriveDataTypeable #-}

import Data.Conduit (runConduitRes, (.|))
import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Conduit (simpleHttp)
import Network.HTTP.Client.Conduit (parseUrlThrow)
import Network.HTTP.Simple (httpSource, Request, getResponseBody)
import qualified Data.Conduit as C
import Network.URI
import Control.Concurrent.Async
import Text.XML.Light
import Text.XML.Light.Input
import Text.XML.Light.Proc
import Data.Yaml.YamlLight
import Text.Groom
import Text.Printf (printf)
import Data.Either
import Data.Maybe
import Data.Map as DM
import Data.List.Split
import Data.List
import Data.Typeable
import System.FilePath
import System.Directory
import System.IO
import qualified Data.ByteString.Char8 as BS8
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.Monad.STM
import Control.Concurrent.STM.TChan
import System.Random.Shuffle
import Crypto.Hash.SHA1 as SHA1
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
    indent ind = (P.take ind $ repeat ' ')
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

data GlobalParams = GlobalParams {rootPath :: String} deriving Show

instance Exception YamlException
yamlError str = hoistEither $ Left $ SomeException $ YamlException str

readFeedConfig :: FilePath -> EitherTIO ([FeedSpec], GlobalParams)
readFeedConfig filePath = do
    fileYaml <- (EitherT . try) $ parseYamlFile filePath
    let top_level_error = yamlError "yaml file must have 'feeds' and 'config' at top level"
    fileMap <- case fileYaml of 
        (YMap fileMap') -> return fileMap'
        _ -> top_level_error
    globalParamsYaml <- case DM.lookup (yPack "config") fileMap of 
        (Just globalParamsYaml') -> return globalParamsYaml'
        _ -> top_level_error
    allSpecsYaml <- case DM.lookup (yPack "feeds") fileMap of 
        (Just allSpecsYaml') -> return allSpecsYaml'
        _ -> top_level_error
    specEntries <- yamlToSpecEntries allSpecsYaml
    feedSpecs <- mapM specEntryToFeedSpec specEntries
    globalParams <- yamlToGlobalParams globalParamsYaml
    return (feedSpecs, globalParams)
        where 
            yPack :: [Char] -> YamlLight
            yPack = YStr . BS8.pack
            yLookup :: [Char] -> (Map YamlLight YamlLight) -> Maybe [Char]
            yLookup key map = do
                yaml <- DM.lookup (yPack key) map
                case yaml of
                    YStr str -> return $ BS8.unpack str
                    _ -> Nothing
                
            yamlToSpecEntries :: YamlLight -> EitherTIO [(YamlLight, YamlLight)]
            yamlToSpecEntries (YMap entryMap) = return $ toList entryMap where
            yamlToSpecEntries _ = yamlError "Badly formatted feed specifications."

            yamlToGlobalParams :: YamlLight -> EitherTIO GlobalParams
            yamlToGlobalParams globalParamsYaml = get $ lookupYL (yPack "root_path") globalParamsYaml >>= unStr >>= return . BS8.unpack where
                get Nothing = yamlError "Badly formatted feed specifications."
                get (Just dirtyRootPath) = do
                    rootPath <- sanitizeRootPath dirtyRootPath
                    return $ GlobalParams rootPath

            specEntryToFeedSpec :: (YamlLight, YamlLight) -> EitherTIO FeedSpec
            specEntryToFeedSpec ((YStr feedName), (YMap entriesMap)) = do
                -- either, not eithert. hmm.
                url <- errToEitherT . fromJust $ yLookup "url" entriesMap
                let fileInfoGetterName = yLookup "itemNodeToFileInfo" entriesMap
                let maxEntriesToGetStr = yLookup "maxEntriesToGet" entriesMap
                feedRelPath <- sanitizePath $ yLookup "path" entriesMap
                let maxEntriesToGet = maxEntriesToGetStr >>= fmap fst . listToMaybe . reads

                return FeedSpec {
                  feedName = BS8.unpack feedName,
                  rssFeedURL = url,
                  feedRelPath = feedRelPath,
                  itemNodeToFileInfo = fileInfoGetterName >>= getFileInfoGetter,
                  maxEntriesToGet = maxEntriesToGet 
                }
            specEntryToFeedSpec _ = yamlError $ "Badly formatted yaml entry. Are you missing a field?"

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
data ContentFileJob = ContentFileJob' {
    contentFileJobRSSEntry :: RSSEntry,
    contentFileJobRequest  :: Request,
    contentFileJobFilePath :: FilePath
    }

-- Given an RSS Feed, grab the latest entries
getLatestEntries :: RSSFeed -> [RSSEntry]
getLatestEntries rssFeed = P.take (fromMaybe 5 $ maxEntriesToGet $ rssFeedSpec rssFeed) $ rssFeedEntries rssFeed

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
    putStrLn $ "Downloading Feed: " ++ (rssFeedURL rssSpec)
    hFlush stdout
    feedData <- simpleHttp $ rssFeedURL rssSpec
    let content = (parseXML feedData)
    let rssFeed = RSSFeed rssSpec (getRSSEntries content rssSpec) content
    case getLatestEntries rssFeed of
        [] -> error "Empty Feed. Check the file format?"
        _  -> return rssFeed

----
-- Content File Getting
----

-- Given an RSSEntry, and a file name, create a ContentFileJob structure
getContentFileJob :: (RSSEntry, String) -> IO (Either RSSEntryError ContentFileJob) 
getContentFileJob (rssEntry, fileName) = do
    eitherRequest <- try $ parseUrlThrow $ rssEntryURL rssEntry
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
            putStrLn $ "Downloading Content: " ++ finalContentFilePath
            hFlush stdout
            createDirectoryIfMissing True $ takeDirectory finalContentFilePath
            download (contentFileJobRequest contentFileJob) tmpContentFilePath
            renameFile tmpContentFilePath finalContentFilePath
            return ()
        True -> do
            putStrLn $ "Already Have: " ++ finalContentFilePath
            hFlush stdout
        where
            download request path = runConduitRes $ httpSource request getResponseBody
                    .| sinkFile path

alphaNumeric = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

-- Given a potentially dirty name (probably taken from a URL), replace any
-- characters that are not alphanumeric, . or - with _. 
sanitizeForFileName :: String -> String
sanitizeForFileName "" = "item"
sanitizeForFileName raw_file_name = P.map sanitizeChar raw_file_name where
    sanitizeChar char
        | not $ elem char (alphaNumeric ++ "-") = '_'
        | otherwise = char

-- Given a relative path for the feed to go into, create a cleaned up version,
-- and throw an error for any characters not alphanumeric, _ or /. Somewhat
-- restrictive
sanitizePath :: Maybe String -> EitherTIO (Maybe String) 
sanitizePath Nothing = return Nothing
sanitizePath (Just dirtyPath) = validate $ stripRev $ stripRev $ dirtyPath where
    strip ('/':dirtyPath') = strip dirtyPath'
    strip dirtyPath' = dirtyPath'
    stripRev :: String -> String
    stripRev = strip . reverse
    validate dirtyPath'
        | length dirtyPath' == 0 = yamlError "path should have a directory name"
        | not $ all (flip elem (alphaNumeric ++ "_/")) dirtyPath' = yamlError $ dirtyPath' ++ ": path can only be alphanumerics and slashes" 
        | otherwise = return $ Just dirtyPath'

sanitizeRootPath :: String -> EitherTIO String
sanitizeRootPath dirtyPath = do
    strippedPath <- sanitizePath $ Just dirtyPath
    case dirtyPath of -- if the original started with '/', put it back after sanitizing
        ('/':_) -> return $ '/':(fromJust strippedPath)
        _ -> return $ fromJust strippedPath

-- Given an RSS Entry, return a file name based purely on the URL of the entry
getContentFileName :: RSSEntry -> String
getContentFileName rssEntry = normalize_extension $ cleanBase $ cleanExt raw_file_name where
    cleanBase raw_file_name = replaceBaseName  raw_file_name $ P.take 140 $ sanitizeForFileName $ takeBaseName raw_file_name
    cleanExt raw_file_name  = replaceExtension raw_file_name $ P.take 10  $ sanitizeForFileName $ (tail . takeExtension) raw_file_name

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
getContentFilePath :: RSSEntry -> GlobalParams -> FilePath
getContentFilePath rssEntry globalParams = path where
    path = P.foldl combine "/" [
        rootPath globalParams,
        addRootDir contentFileExtraDir,
        getContentFileName rssEntry]
    fileDir = (sanitizeForFileName . feedName . rssEntryFeedSpec) rssEntry
    contentFileExtraDir = (feedRelPath . rssEntryFeedSpec) rssEntry
    addRootDir Nothing = fileDir
    addRootDir (Just extraDir) = combine extraDir fileDir

-- Credit http://stackoverflow.com/a/9503170
toHex :: BS8.ByteString -> String
toHex bytes = BS8.unpack bytes >>= printf "%02x"

-- Given a list of file names with potential collisions, return the list with
-- unique names generated for any path collisions
getUniqueFileNames :: [RSSEntry] -> GlobalParams -> [FilePath]
getUniqueFileNames entries globalParams = P.foldl uniquify [] $ reverse entries where
    all_names = P.map (flip getContentFilePath globalParams) entries
    uniquify :: [FilePath] -> RSSEntry -> [FilePath]
    uniquify names_so_far entry = uniqueName:names_so_far where
        name = getContentFilePath entry globalParams
        uniqueName
            | (length $ P.filter (== name) all_names) > 1 = (
                replaceBaseName name 
                $ (takeBaseName name) 
                    ++ '.':(toHex $ SHA1.hash $ BS8.pack $ rssEntryURL entry)
            )
            | otherwise = name

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
get_content_files :: [RSSEntry] -> GlobalParams -> IO ([RSSEntry], [RSSEntryError])
get_content_files orderedEntries globalParams = do 
    entries <- shuffleM orderedEntries
    let entriesFilenames = getUniqueFileNames entries globalParams

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
    putStrLn "\n"
    putStrLn $ "Item Nodes: "
    putStrLn $ P.foldl (++) "" $ P.map prettyShowNodes allItemNodes
    putStrLn ""
    hFlush stdout

    return ()

-- Debug: Display the FeedSpecs derived from the yaml file
debug_yaml_reading :: [FeedSpec] -> IO ()
debug_yaml_reading feedSpecs = do
    putStrLn $ groom feedSpecs
    hFlush stdout


-- Debug: Display (Entry URL) for each entry
debug_entry_urls :: [RSSEntry] -> IO ()
debug_entry_urls entries = do
    putStrLn "\n"
    putStrLn $ "RSS Entries:\n" ++ groom ( P.map rssEntryURL entries )
    putStrLn ""
    hFlush stdout

-- Debug: Display (Entry URL, Entry File Path) for each entry
debug_entry_urls_file_paths :: [RSSEntry] -> GlobalParams -> IO ()
debug_entry_urls_file_paths entries globalParams = do
    putStrLn "\n"
    putStrLn $ "All Entry URLs/Content File Paths, from entries: \n" ++ (
        groom $ zip (P.map rssEntryURL entries) (getUniqueFileNames entries globalParams))
    putStrLn ""
    hFlush stdout


debug_entry_successes :: [RSSEntry] -> IO ()
debug_entry_successes successRSSEntries = do
    putStrLn "\n"
    putStrLn $ "RSS Content File Successes (Including skips from already having them):" ++ ( groom $ P.map rssEntryURL successRSSEntries )
    putStrLn ""
    hFlush stdout

-- Debug: Show errors in downloading rss files.
debug_feed_file_errors :: [FeedSpecError] -> IO ()
debug_feed_file_errors errorFeedSpecs = do
    let showErr (feedSpec, error) = (feedName feedSpec, (P.take 100 $ show error) ++ "...")

    putStrLn "\n"
    putStrLn $ "RSS Feed File Errors:\n" ++ (groom $ P.map showErr errorFeedSpecs)
    putStrLn ""
    hFlush stdout


debug_entry_errors :: [RSSEntryError] -> IO ()
debug_entry_errors errorRSSEntries = do
    let showErr (entry, error) = (rssEntryURL entry, (P.take 100 $ show error) ++ "...")

    putStrLn "\n"
    putStrLn $ "RSS Content File Errors:\n" ++ (groom $ P.map showErr errorRSSEntries)
    putStrLn ""
    hFlush stdout

----
-- Main
----

main :: IO ()
main = do
    result <- runEitherT $ do
        (feedSpecs, globalParams) <- readFeedConfig "feeds.yaml"
        lift $ do
            (successRSSFeeds, errorFeedSpecs, entries) <- get_feeds feedSpecs
            (successEntries, errorEntries) <- get_content_files entries globalParams
            -- move ot True case as is useful for verbosity
            case True of
                True -> do -- enabled debug
                    debug_feed_file_errors errorFeedSpecs
                    debug_entry_errors errorEntries
                False -> do -- disabled debug, but still subject to type checking
                    debug_yaml_reading feedSpecs
                    debug_entry_urls entries
                    debug_entry_urls_file_paths entries globalParams
                    debug_inspect_feed_file successRSSFeeds
                    debug_entry_successes successEntries

            return ()
    case result of
        Left a -> do
            putStrLn $ groom a
            hFlush stdout
        _ -> return ()

    return ()
