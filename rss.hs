import Network.HTTP.Conduit
import Network.URI
import Control.Concurrent.Async
import Text.XML.Light
import Text.XML.Light.Input
import Text.XML.Light.Proc
import Data.Either
import Data.Maybe
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
    show' ind (SElementRepr name attrs subelems) = indent ind ++ name ++ " - " ++ attrDisp ++ "\n" ++ subElemDisp where
        ind' = indent ind
        attrDisp = concatMap showAttr attrs
        subElemDisp = concatMap (show' (ind + 2)) subelems
    show' ind (STextRepr str) = indent ind ++ "\"" ++ str ++ "\"\n"
    show' _ SDontCareRepr = ""
    showAttr (a, b) = a ++ "=" ++ "\"" ++ b ++ "\"" ++ " "

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
type OSPath = String
type URL = String
type FeedFile = String

-- RSS

-- what I define
data FeedSpec = FeedSpec {feedName :: String, rssFeedURL :: URL, feedMaxFiles :: Int, feedRelPath :: Maybe OSPath}

-- what comes out in reality, from my definition or from the world
data RSSFeed = RSSFeed {rssFeedSpec :: FeedSpec, rssFeedEntries :: [RSSEntry], xmlContent :: [Content]}
data RSSEntry = RSSEntry {rssEntryFeedSpec :: FeedSpec, rssEntryTitle :: Maybe String, rssEntryURL :: URL}
data ContentFile = ContentFile {content :: ContentData, contentRSSEntry :: RSSEntry}

getItemNodes top_elements = downXMLPath ["rss", "channel", "item"] (onlyElems top_elements)

getRSSEntries :: [Content] -> FeedSpec -> [RSSEntry]
getRSSEntries top_elements rssSpec = entries where
    items = concatMap (filterElements (isJust . getURL)) $ getItemNodes top_elements
    getURL item = (Just item) >>= (findChild' "encloure") >>= (findAttr' "url")

    entries = [
            RSSEntry {
            rssEntryTitle=fmap strContent $ findChild' "title" item,
            rssEntryURL=fromJust $ getURL item,
            rssEntryFeedSpec=rssSpec
        } 
        | item <- items ]

getContentFile rssEntry = do
    content <- simpleHttp $ rssEntryURL rssEntry
    return ContentFile {
    content=content,
    contentRSSEntry=rssEntry
    }

saveContentFile contentFile = BSLazy.writeFile (getContentFilePath contentFile) (content contentFile)

getContentFilePath contentFile = map sanitize file_name where
    -- let this error for now
    file_name = (uriPath . fromJust . parseURI . rssEntryURL . contentRSSEntry) contentFile
    sanitize char
        | not $ elem char (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ".-") = '_'
        | otherwise = char

-- tmp file?

getRSSFeed :: FeedSpec -> IO RSSFeed
getRSSFeed rssSpec = do
    feedData <- simpleHttp $ rssFeedURL rssSpec
    let content = (parseXML feedData)
    return $ RSSFeed rssSpec (getRSSEntries content rssSpec) content

-- test data
feeds = [
    FeedSpec "Free Talk Live" "http://feeds.feedburner.com/ftlradio" 2 Nothing,
    FeedSpec "Awkward Fist Bump" "http://awkwardfistbump.libsyn.com/rss" 2 $ Just "awk/ward",
    FeedSpec "Nope" "bad_one" 2 Nothing
    ]

main = do
    rssThreads <- mapM (async . getRSSFeed) feeds
    rssFeeds <- mapM waitCatch rssThreads

    putStr "\n\n"
    putStr $ "RSS Feed File Errors: " ++ ( show $ lefts rssFeeds )
    putStr "\n\n"

    fileThreads <- mapM (async . getContentFile) $ concatMap rssFeedEntries $ rights rssFeeds
    files <- mapM waitCatch fileThreads
    
    putStr "\n\n"
    putStr $ "RSS Content File Errors: " ++ ( show $ lefts files )
    putStr "\n\n"

    putStr "\n\n"
    putStr $ "RSS Success Content File URLs: " ++ (show $ map getContentFilePath $ rights files)
    putStr "\n\n"

    let allItemNodes = rights rssFeeds >>= getItemNodes . xmlContent >>= return . simpleXML'
    putStr "\n\n"
    putStr $ "Item Nodes: "
    putStr (prettyShow $ head allItemNodes)
    putStr "\n\n"

    return ()
