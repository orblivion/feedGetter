import Network.HTTP.Conduit
import Control.Concurrent.Async
import Text.XML.Light
import Text.XML.Light.Input
import Text.XML.Light.Proc
import Data.Either
import Data.Maybe

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

-- RSS

-- what I define
data FeedSpec = FeedSpec {feedName :: String, feedMaxFiles :: Int, feedRelPath :: Maybe String}

type ContentData = String
type OSPath = String
type FeedFile = String

-- what comes out in reality, from my definition or from the world
data RSSFeed = RSSFeed {rssFeedSpec :: FeedSpec, rssFeedEntries :: [RSSEntry] }
data RSSEntry = RSSEntry {rssEntryFeedSpec :: FeedSpec, rssEntryTitle :: String, rssEntryURL :: String}
data ContentFile = ContentFile {content :: ContentData, destinationPath :: OSPath}

getRSSEntries :: [Element] -> RSSSpec -> [RSSEntry]
getRSSEntries top_elements rssSpec = entries where
    items = concatMap (filterElements hasLink) allItems where
	allItems = downXMLPath ["rss", "channel", "item"] (onlyElems $ top_elements)
	hasLink item = isJust $ findChild (unqual "link") item

    entries = [
	RSSEntry {
	    rssEntryTitle=defaultingChildVal "title" "Unknown" item,
	    rssEntryURL=sureChildVal "link" item
	    rssEntryFeedSpec=rssSpec
	} 
	| item <- items ]

getContentFiles :: RSSFeed -> IO ContentFile
getContentFiles (RSSFeed feedSpec entries) = do
    return contentThreads
    return $ ContentFile {content=content, destinationPath=fromMaybe "" (feedRelPath feedSpec)}

getContentFile rssEntry = do
    content <- simpleHttp $ rssEntryURL rssEntry
    return ContentFile {
	content=content,
	destinationPath=fromMaybe "" (feedRelPath . rssEntryFeedSpec) rssEnty
    }

getRSSFeed :: FeedSpec -> IO RSSFeed
getRSSFeed rssSpec = do
    feedData <- (async . simpleHttp) $ rssEntryURL rssSpec
    return $ RSSFeed rssSpec $ getRSSEntries feedData rssSpec

-- test data
feeds = [
	FeedSpec "http://feeds.feedburner.com/ftlradio" 2 Nothing,
	FeedSpec "http://awkwardfistbump.libsyn.com/rss", 2, "awk/ward",
	FeedSpec "bad_one" 2 Nothing
    ]

main = do
    rssThreads <- mapM (async . getRSSFeed) feeds
    rssFeeds <- mapM waitCatch rssThreads
    -- handle lefts somehow

    fileThreads <- mapM (async . getContentFile) $ concatMap rssEntries rssFeeds
    files <- mapM waitCatch fileThreads
    -- handle lefts again somehow

    return ()
