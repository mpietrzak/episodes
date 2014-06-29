{-# LANGUAGE OverloadedStrings #-}

module TVRage (
    Show (..),
    FullShowInfo (..),
    Season (..),
    Episode (..),
    main,
    searchShows,
    getFullShowInfo
) where


import Control.Applicative
import Data.Text (Text)
import Options
import Prelude hiding (Show, shows)
import Text.XML.Cursor (($//), ($/), ($.//), (&/), (&.//), (&|))
import Data.Maybe (catMaybes)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Debug.Trace as Trace
import qualified Network.HTTP as HTTP
import qualified Network.URI as NU
import qualified Prelude as Prelude
import qualified Text.XML as XML
import qualified Text.XML.Cursor as XML


data MainOptions = MainOptions {}


data SearchOptions = SearchOptions
    { searchOptionsQuery :: Maybe String }


data FullShowInfoOptions = FullShowInfoOptions
    { fullShowInfoOptionsShowId :: Maybe Integer }


data Show = Show
    { showTitle :: Text
    , showTVRageId :: Integer }
    deriving (Prelude.Show)


data Episode = Episode
    { episodeNumber :: Integer
    , episodeTitle :: Text }
    deriving (Prelude.Show)


data Season = Season
    { seasonNumber :: Integer
    , seasonEpisodes :: [Episode] }
    deriving (Prelude.Show)


data FullShowInfo = FullShowInfo
    { fullShowInfoTitle :: Text
    , fullShowInfoTVRageId :: Integer
    , fullShowInfoTotalSeasons :: Integer
    , fullShowInfoSeasons :: [Season] }
    deriving Prelude.Show


instance Options MainOptions where
    defineOptions = pure MainOptions


instance Options SearchOptions where
    defineOptions = pure SearchOptions
        <*> simpleOption "show" Nothing ""


instance Options FullShowInfoOptions where
    defineOptions = pure FullShowInfoOptions
        <*> simpleOption "show-id" Nothing ""


traceValue :: Prelude.Show x => String -> x -> x
traceValue msg x =
    Trace.trace (msg ++ ": " ++ (show x)) x


getRequest :: String -> HTTP.Request BL.ByteString
getRequest urlString =
  case NU.parseURI urlString of
    Nothing -> error ("getRequest: Not a valid URL - " ++ urlString)
    Just u  -> HTTP.mkRequest HTTP.GET u


-- Fetch HTTP resource, return as lazy byte string.
httpGet :: String -> IO (BL.ByteString)
httpGet url = do
    let req = getRequest url :: HTTP.Request BL.ByteString
    rsp <- HTTP.simpleHTTP req
    HTTP.getResponseBody rsp


parseSearchResponse :: BL.ByteString -> [Show]
parseSearchResponse resp =
    case parseResult of
        Left _ -> []
        Right doc -> filter (\s -> showTVRageId s /= 0) $ map showElementToShow (findShowElements doc)
    where
        parseResult = XML.parseLBS XML.def resp
        showElementToShow showElement = Show { showTitle=_title, showTVRageId=_tvrId }
            where
                _title = T.concat $ elementCursor $/ XML.element "name" &/ XML.content
                _tvrIdText = T.concat $ elementCursor $/ XML.element "showid" &/ XML.content
                _tvrId = case T.decimal _tvrIdText of
                    Left _ -> 0
                    Right (_tvrIdInt, _) -> _tvrIdInt
                elementCursor = XML.fromNode showElement :: XML.Cursor
        findShowElements doc = map XML.node (XML.fromDocument doc $// XML.element "show")


searchShows :: Text -> IO [Show]
searchShows search = parseSearchResponse <$> httpGet url
    where url = "http://services.tvrage.com/feeds/search.php?show=" ++ (T.unpack search)


extractEpisodeFromEpisodeCurs :: XML.Cursor -> Maybe Episode
extractEpisodeFromEpisodeCurs _curs = _maybeEpisode
    where
        _episodeTitle = traceValue "_episodeTitle" $ T.concat $ _curs $/ XML.element "title" &/ XML.content
        _episodeNumberText = T.concat $ _curs $/ XML.element "epnum" &/ XML.content
        _maybeEpisodeNumber = traceValue "_maybeEpisodeNumber" $ case (T.decimal _episodeNumberText) of
            Left _ -> Nothing
            Right (_n, _) -> Just _n
        _maybeEpisode = case _maybeEpisodeNumber of
            Nothing -> Nothing
            Just _n -> Just Episode
                { episodeNumber = _n
                , episodeTitle = _episodeTitle }


extractSeasonEpisodesFromSeasonCurs :: XML.Cursor -> [Episode]
extractSeasonEpisodesFromSeasonCurs _seasonCurs =
    catMaybes $ map extractEpisodeFromEpisodeCurs _episodeCursors
    where
        _episodeCursors = _seasonCurs $/ XML.element "episode"


extractSeasonFromSeasonCurs :: XML.Cursor -> Maybe Season
extractSeasonFromSeasonCurs _curs = _maybeSeason
    where
        _seasonNumberText = T.concat $ XML.attribute "no" _curs
        _maybeSeasonNumber = Trace.trace ("_seasonNumberText: " ++ (show _seasonNumberText)) $ case (T.decimal _seasonNumberText) of
            Left _ -> Nothing
            Right (_n, _) -> Just _n
        _episodes = extractSeasonEpisodesFromSeasonCurs _curs
        _maybeSeason = case _maybeSeasonNumber of
            Nothing -> Nothing
            Just _n -> Just Season
                { seasonNumber = _n
                , seasonEpisodes = _episodes }


extractSeasonsFromFullShowInfoCurs :: XML.Cursor -> [Season]
extractSeasonsFromFullShowInfoCurs fullShowInfoCurs =
    catMaybes $ map extractSeasonFromSeasonCurs seasonCursors
    where
        seasonCursors = fullShowInfoCurs $// XML.element "Season" :: [XML.Cursor]


parseGetFullShowInfoResponse :: Integer -> BL.ByteString -> Maybe FullShowInfo
parseGetFullShowInfoResponse tvRageId resp =
    case parseResult of
        Left _ -> Nothing
        Right doc -> Just FullShowInfo
            { fullShowInfoTitle = _title
            , fullShowInfoTVRageId = tvRageId
            , fullShowInfoTotalSeasons = _seasonCount
            , fullShowInfoSeasons = _seasons }
                where
                    _curs = XML.fromDocument doc
                    _title = T.concat $ _curs $// XML.element "name" &/ XML.content
                    _seasonCountText = T.concat $ _curs $/ XML.element "totalseasons" &/ XML.content
                    _seasonCount = case (T.decimal _seasonCountText) of
                        Left _ -> 0
                        Right (i, _) -> i
                    _seasons = extractSeasonsFromFullShowInfoCurs _curs
    where
        parseResult = XML.parseLBS XML.def resp


getFullShowInfo :: Integer -> IO (Maybe FullShowInfo)
getFullShowInfo showId = parseGetFullShowInfoResponse showId <$> httpGet url
    where url = "http://services.tvrage.com/feeds/full_show_info.php?sid=" ++ (show showId)


searchCommand :: MainOptions -> SearchOptions -> [String] -> IO ()
searchCommand _ searchOpts _ = do
    let _mshow = searchOptionsQuery searchOpts
    case _mshow of
        Just _show -> do
            shows <- searchShows (T.pack _show)
            putStrLn $ "shows: " ++ show shows
        Nothing -> error "Show is required"


fullShowInfoCommand :: MainOptions -> FullShowInfoOptions -> [String] -> IO ()
fullShowInfoCommand _ _fsiOpts _ = do
    let _mShowId = fullShowInfoOptionsShowId _fsiOpts
    case _mShowId of
        Just _showId -> do
            _fullShowInfo <- getFullShowInfo _showId
            putStrLn $ "full show info: " ++ show _fullShowInfo
        Nothing -> error "Show ID is required"


main :: IO ()
main = runSubcommand
    [ subcommand "search" searchCommand
    , subcommand "full-show-info" fullShowInfoCommand
    ]

