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
import Data.Typeable
import System.FilePath
import System.Directory
import qualified Data.ByteString.Lazy.Internal as BSI
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
-- XML Utilities
----

unqualifyfy f = f' where
    f' a = f (unqual a)
findElements' = unqualifyfy findElements
findChild' = unqualifyfy findChild
findAttr' = unqualifyfy findAttr

data SimpleXMLRepr = SElementRepr String [(String, String)] [SimpleXMLRepr] | STextRepr String | SDontCareRepr
simpleXML' e = simpleXML $ Elem e
simpleXML (Elem e) = SElementRepr name attrs content where
    name = (qName . elName) e
    attrs = P.map simpleAttr (elAttribs e)
    content = P.map simpleXML (elContent e)
    simpleAttr attr = (qName $ attrKey attr, attrVal attr)
simpleXML (Text e) = STextRepr $ cdData e
simpleXML _ = SDontCareRepr

prettyShow s = show' 0 s where
    indent ind = (take ind $ repeat ' ')
    show' ind (SElementRepr name attrs subelems) = indent ind ++ name ++ "\n" ++ attrDisp attrs ++ "\n" ++ subElemDisp where
        attrDisp [] = ""
        attrDisp attrs = concatMap showAttr attrs
        showAttr (a, b) = indent ind ++ "  " ++ a ++ "=" ++ "\"" ++ b ++ "\"" ++ "\n"
        subElemDisp = concatMap (show' (ind + 2)) subelems
    show' ind (STextRepr str) = indent ind ++ "\"" ++ str ++ "\"\n"
    show' _ SDontCareRepr = ""

downXMLPath' [] elems = elems
downXMLPath' tag_name elems = concatMap (findElements' tag_name) $ elems

downXMLPath :: [String] -> [Element] -> [Element]
downXMLPath [] = id
downXMLPath (tag_name:next_names) = (downXMLPath next_names) . (downXMLPath' tag_name)

defaultingChildVal :: String -> String -> Element -> String
defaultingChildVal name default_val elem = fromMaybe default_val (getVal elem) where
    getVal elem = do
    child <- findChild' name elem
    return $ strContent child


----
-- Assorted - TODO specify
----

-- Wrap any operation that throws an exception to an EitherT
errToEitherT :: a -> EitherT SomeException IO a
errToEitherT = EitherT . try . return

type EitherTIO a = EitherT SomeException IO a

type ContentData = BSI.ByteString
type URL = String
type FeedFile = String

-- The structure of the feeds I specify that I want
type FileInfoGetter = (Element -> Maybe String, Element -> Maybe String)
data FeedSpec = FeedSpec { 
    feedName :: String, 
    rssFeedURL :: URL,
    feedRelPath :: Maybe FilePath,
    itemNodeToFileInfo :: Maybe FileInfoGetter,
    maxEntriesToGet :: Maybe Int
    }


----
-- Reading Yaml configuration
----

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
}
data ContentFileJob m = ContentFileJob {
    contentFileJobRequest :: (Request m),
    contentFileJobFilePath :: FilePath
    }

getLatestEntries rssFeed = take (fromMaybe 5 $ maxEntriesToGet $ rssFeedSpec rssFeed) $ rssFeedEntries rssFeed

getItemNodes top_elements = downXMLPath ["rss", "channel", "item"] (onlyElems top_elements)

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

getRSSFeed :: FeedSpec -> IO RSSFeed
getRSSFeed rssSpec = do
    feedData <- simpleHttp $ rssFeedURL rssSpec
    let content = (parseXML feedData)
    return $ RSSFeed rssSpec (getRSSEntries content rssSpec) content

----
-- Content File Getting
----

getContentFileJob (rssEntry, fileName) = do
    request <- parseUrl $ rssEntryURL rssEntry

    return ContentFileJob {
        contentFileJobRequest = request,
        contentFileJobFilePath = fileName
    }

runContentFileJob contentFileJob = do
    let finalContentFilePath = contentFileJobFilePath contentFileJob
    let tmpContentFilePath = finalContentFilePath ++ "~"
    alreadyHave <- doesFileExist finalContentFilePath
    case alreadyHave of
        True -> do
            putStr $ "Downloading: " ++ contentFileJobFilePath contentFileJob ++ "\n"
            createDirectoryIfMissing True $ takeDirectory finalContentFilePath
            download (contentFileJobRequest contentFileJob) tmpContentFilePath
            renameFile tmpContentFilePath finalContentFilePath
            return ()
        False -> do
            putStr $ "Already Have: " ++ contentFileJobFilePath contentFileJob ++ "\n"
        where
            download request path = do
                withManager $ \manager -> do
                    response <- http request manager
                    responseBody response C.$$+- sinkFile path

alphaNumeric = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

sanitizeForFileName "" = "item"
sanitizeForFileName raw_file_name = P.map sanitizeChar raw_file_name where
    sanitizeChar char
        | not $ elem char (alphaNumeric ++ ".-") = '_'
        | otherwise = char

sanitizeFeedRelPath :: Maybe String -> EitherTIO (Maybe String) 
sanitizeFeedRelPath Nothing = return Nothing
sanitizeFeedRelPath (Just str) = validate $ stripRev $ stripRev $ str where
    strip ('/':str) = strip str
    strip str = str
    stripRev :: String -> String
    stripRev = strip . reverse
    validate str
        | length str == 0 = yamlError "feedRelPath should have a directory name"
        | not $ all (flip elem (alphaNumeric ++ "-/")) str = yamlError "feedRelPath can only be alphanumerics and slashes" 
        | otherwise = return $ Just str

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

getContentFilePath rssEntry = path where
    path = P.foldl combine "/" [
        rootPath,
        addRootDir contentFileExtraDir,
        getContentFileName rssEntry]
    fileDir = (sanitizeForFileName . feedName . rssEntryFeedSpec) rssEntry
    contentFileExtraDir = (feedRelPath . rssEntryFeedSpec) rssEntry
    addRootDir Nothing = fileDir
    addRootDir (Just extraDir) = combine extraDir fileDir

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

getUniqueFileNames = getUniqueFileNames' . (P.map getContentFilePath)

rootPath = "/home/haskell/feeds"

----
-- Processes
----

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

-- process_content_file_jobs is one content-file-getting thread. An arbitrary
-- amount can be spawned operating on the same channels
maxContentFileThreads = 5
process_content_file_jobs jobChan resultChan = relayTChan jobChan resultChan run where
    run job = try $ runContentFileJob job :: IO (Either SomeException ())

get_feeds :: [FeedSpec] -> IO ([Either SomeException RSSFeed], [RSSEntry])
get_feeds feedSpecs = do
    rssThreads <- mapM (async . getRSSFeed) feedSpecs
    rssFeeds <- mapM waitCatch rssThreads

    let entries = rights rssFeeds >>= getLatestEntries

    return (rssFeeds, entries)

get_content_files orderedEntries = do 
    entries <- shuffleM orderedEntries
    let entriesFilenames = getUniqueFileNames entries

    contentFileJobs <- mapM getContentFileJob $ zip entries entriesFilenames

    jobChan <- atomically $ newTChan
    resultChan <- atomically $ newTChan
    mapM (atomically . (writeTChan jobChan)) contentFileJobs

    contentThreads <- mapM async $ replicate maxContentFileThreads $ process_content_file_jobs jobChan resultChan
    mapM waitCatch contentThreads
    contentFileResults <- collectTChan resultChan

    let contentFiles = rights contentFileResults

    putStr "\n\n"
    putStr $ "RSS Content File Errors: " ++ ( groom $ lefts contentFileResults )
    putStr "\n\n"

    putStr "\n\n"
    putStr $ "Success Content File Paths:\n" ++ (groom entriesFilenames)
    putStr "\n\n"

    return contentFiles

----
-- Debug Functions
----

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

-- Debug: Displays a representation of the RSS file, for ease of finding useful elements
debug_inspect_feed_file :: [Either SomeException RSSFeed] -> IO ()
debug_inspect_feed_file rssFeeds = do
    let allItemNodes = rights rssFeeds >>= getItemNodes . xmlContent >>= return . simpleXML'
    putStr "\n\n"
    putStr $ "Item Nodes: \n"
    putStr (prettyShow $ head allItemNodes)
    putStr "\n\n"   

    return ()

-- Debug: Show errors in downloading rss files.
debug_feed_download_errors :: [Either SomeException RSSFeed] -> [FeedSpec] -> IO ()
debug_feed_download_errors rssFeeds feedSpecs = do
    let failedFeedSpecs = P.map snd $ P.filter (isError . fst) $ zip rssFeeds feedSpecs where
        isError (Right a) = False
        isError (Left a) = True

    putStr "\n\n"
    putStr $ "RSS Feed File Errors:\n" ++ ( groom $ zip (P.map feedName failedFeedSpecs) (lefts rssFeeds) )
    putStr "\n\n"


----
-- Main
----

main :: IO ()
main = do
    result <- runEitherT $ do
        feedSpecs <- readFeedConfig "feeds.yaml"
        lift $ do
            (rssFeeds, entries) <- get_feeds feedSpecs

            -- uncomment as is useful for verbosity
            -- debug_feed_download_errors rssFeeds feedSpecs
            -- debug_feed_download_errors rssFeeds feedSpecs
            -- debug_entry_urls entries
            -- debug_entry_urls_file_paths entries
            -- debug_inspect_feed_file rssFeeds

            files <- get_content_files entries

            return ()
    case result of
        Left a -> putStrLn $ groom a
        _ -> return ()

    return ()
