import Network.HTTP.Conduit
import Control.Concurrent.Async
import Text.XML.Light
import Text.XML.Light.Input
import Text.XML.Light.Proc
import Data.Either
import Data.Maybe
import qualified Data.ByteString.Lazy.Internal as BSInternal
import qualified Data.ByteString.Lazy as BSLazy

-- XML
downXMLPath' [] elems = elems
downXMLPath' tag_name elems = concatMap (findElements $ unqual tag_name) $ elems

downXMLPath :: [String] -> [Element] -> [Element]
downXMLPath [] = id
downXMLPath (tag_name:next_names) = (downXMLPath next_names) . (downXMLPath' tag_name)

sureChildVal name elem = strContent $ fromJust $ findChild (unqual "name") elem
defaultingChildVal :: String -> String -> Element -> String
defaultingChildVal name default_val elem = fromMaybe default_val (getVal elem) where
    getVal elem = do
    child <- findChild (unqual name) elem
    return $ strContent child

type ContentData = BSInternal.ByteString
type OSPath = String
type URL = String
type FeedFile = String

-- RSS

-- what I define
data FeedSpec = FeedSpec {feedName :: String, rssFeedURL :: URL, feedMaxFiles :: Int, feedRelPath :: Maybe OSPath}

-- what comes out in reality, from my definition or from the world
data RSSFeed = RSSFeed {rssFeedSpec :: FeedSpec, rssFeedEntries :: [RSSEntry] }
data RSSEntry = RSSEntry {rssEntryFeedSpec :: FeedSpec, rssEntryTitle :: String, rssEntryURL :: URL}
data ContentFile = ContentFile {content :: ContentData, contentRSSEntry :: RSSEntry}

getRSSEntries :: [Content] -> FeedSpec -> [RSSEntry]
getRSSEntries top_elements rssSpec = entries where
    items = concatMap (filterElements hasLink) allItems where
        allItems = downXMLPath ["rss", "channel", "item"] (onlyElems top_elements)
    hasLink item = isJust $ findChild (unqual "link") item

    entries = [
            RSSEntry {
            rssEntryTitle=defaultingChildVal "title" "Unknown" item,
            rssEntryURL=sureChildVal "link" item,
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

getContentFilePath contentFile = "tmp"

-- tmp file?

getRSSFeed :: FeedSpec -> IO RSSFeed
getRSSFeed rssSpec = do
    feedData <- simpleHttp $ rssFeedURL rssSpec
    return $ RSSFeed rssSpec $ getRSSEntries (parseXML feedData) rssSpec

-- test data
feeds = [
    FeedSpec "Free Talk Live" "http://feeds.feedburner.com/ftlradio" 2 Nothing,
    FeedSpec "Awkward Fist Bump" "http://awkwardfistbump.libsyn.com/rss" 2 $ Just "awk/ward",
    FeedSpec "Nope" "bad_one" 2 Nothing
    ]

main = do
    rssThreads <- mapM (async . getRSSFeed) feeds
    rssFeeds <- mapM waitCatch rssThreads
    -- handle lefts somehow

    fileThreads <- mapM (async . getContentFile) $ concatMap rssFeedEntries $ rights rssFeeds
    files <- mapM waitCatch fileThreads
    -- handle lefts again somehow

    return ()
