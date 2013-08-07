import Network.HTTP.Conduit
import Network.URI
import Control.Concurrent.Async
import Text.XML.Light
import Text.XML.Light.Input
import Text.XML.Light.Proc
import Data.Either
import Data.Maybe
import Data.List.Split
import System.FilePath
import qualified Data.ByteString.Lazy.Internal as BSInternal
import qualified Data.ByteString.Lazy as BSLazy

-- XML
unqualifyfy f = f' where
    f' a = f (unqual a)
findElements' = unqualifyfy findElements
findChild' = unqualifyfy findChild
findAttr' = unqualifyfy findAttr

data SimpleXMLRepr = SElementRepr String [(String, String)] [SimpleXMLRepr] | STextRepr String | SDontCareRepr
simpleXML' e = simpleXML $ Elem e
simpleXML (Elem e) = SElementRepr name attrs content where
    name = (qName . elName) e
    attrs = map simpleAttr (elAttribs e)
    content = map simpleXML (elContent e)
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


-- Assorted

type ContentData = BSInternal.ByteString
type URL = String
type FeedFile = String

-- RSS

-- what I define
data FeedSpec = FeedSpec {
    feedName :: String, 
    rssFeedURL :: URL,
    feedMaxFiles :: Int,
    feedRelPath :: Maybe FilePath,
    itemNodeToFileInfo :: (Element -> Maybe String, Element -> Maybe String)
    }

itemNodeToUrl = fst . itemNodeToFileInfo
itemNodeToExtension = snd . itemNodeToFileInfo

-- what comes out in reality, from my definition or from the world
data RSSFeed = RSSFeed {rssFeedSpec :: FeedSpec, rssFeedEntries :: [RSSEntry], xmlContent :: [Content]}
data RSSEntry = RSSEntry {
    rssEntryFeedSpec :: FeedSpec, rssEntryTitle :: Maybe String,
    rssEntryURL :: URL, rssEntryElement :: Element
}
data ContentFile = ContentFile {content :: ContentData, contentRSSEntry :: RSSEntry}

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

downloadContentFile rssEntry = do
    putStr $ "Getting: " ++ rssEntryURL rssEntry ++ "\n"
    
    content <- simpleHttp $ rssEntryURL rssEntry
    return ContentFile {
    content=content,
    contentRSSEntry=rssEntry
    }

saveContentFile contentFile = BSLazy.writeFile (getContentFilePath contentFile) (content contentFile)

getContentFilePath contentFile = getContentFilePath' $ contentRSSEntry contentFile
getContentFilePath' rssEntry = (sanitize . normalize_extension) raw_file_name where
    -- let this error for now if a valid name can't be created (specifically expecting this for 
    -- extensions)
    raw_file_name = (last . (splitOn "/") . uriPath . fromJust . parseURI . rssEntryURL) rssEntry
    sanitizeChar char
        | not $ elem char (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ".-") = '_'
        | otherwise = char
    sanitize "" = "item"
    sanitize raw_file_name = map sanitizeChar raw_file_name
    normalize_extension file_name
        | (length extension > 5) && (length extension < 1) = file_name
        | otherwise = file_name ++ (fromJust . elementToExtension . rssEntryElement) rssEntry
            where
                extension = (snd . splitExtension) file_name 
                elementToExtension = (itemNodeToExtension . rssEntryFeedSpec $ rssEntry)

getRSSFeed :: FeedSpec -> IO RSSFeed
getRSSFeed rssSpec = do
    feedData <- simpleHttp $ rssFeedURL rssSpec
    let content = (parseXML feedData)
    return $ RSSFeed rssSpec (getRSSEntries content rssSpec) content

-- test data
from_enclosure = (getFilePath, getExtension) where
    getFilePath item = (findChild' "enclosure" item) >>= findAttr' "url"
    getExtension item = (findChild' "enclosure" item) >>= findAttr' "type" >>= findExtension where
        findExtension "audio/mpeg" = Just "mp3"
        findExtension "audio/ogg" = Just "ogg"
        findExtension _ = Nothing

feedSpecs = [
        FeedSpec "Free Talk Live" "http://feeds.feedburner.com/ftlradio" 2 Nothing from_enclosure,
        FeedSpec "Awkward Fist Bump" "http://awkwardfistbump.libsyn.com/rss" 2 (Just "awk/ward") from_enclosure,
        FeedSpec "Nope" "bad_one" 2 Nothing from_enclosure
    ]

get_feeds feedSpecs = do
    rssThreads <- mapM (async . getRSSFeed) feedSpecs
    rssFeeds <- mapM waitCatch rssThreads

    let entries = rights rssFeeds >>= rssFeedEntries

    putStr "\n\n"
    putStr $ "RSS Feed File Errors: " ++ ( show $ lefts rssFeeds )
    putStr "\n\n"

    putStr "\n\n"
    -- putStr $ "RSS Entries:" ++ show ( map rssEntryURL entries )
    putStr "\n\n"

    return (rssFeeds, entries)

get_content_files entries = do 
    -- Get content files
    fileThreads <- mapM (async . downloadContentFile) $ entries
    contentFiles <- mapM waitCatch fileThreads

    let files = rights contentFiles
    
    putStr "\n\n"
    putStr $ "RSS Content File Errors: " ++ ( show $ lefts contentFiles )
    putStr "\n\n"

    putStr "\n\n"
    putStr $ "Success Content File Paths: " ++ (show $ map getContentFilePath files)
    putStr "\n\n"

    return files

debug_entry_file_paths entries = do
    putStr "\n\n"
    putStr $ "All Content File Paths, from entries: " ++ (show $ map getContentFilePath' entries)
    putStr "\n\n"

debug_item_nodes rssFeeds = do
    let allItemNodes = rights rssFeeds >>= getItemNodes . xmlContent >>= return . simpleXML'
    putStr "\n\n"
    putStr $ "Item Nodes: \n"
    putStr (prettyShow $ head allItemNodes)
    putStr "\n\n"   

    return ()

main = do
    (rssFeeds, entries) <- get_feeds feedSpecs
    --files <- get_content_files entries
    files <- debug_entry_file_paths entries
    debug_item_nodes rssFeeds

    return ()
