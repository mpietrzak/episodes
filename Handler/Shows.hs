{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
module Handler.Shows where


import           Import

import           Control.Applicative ((<*))
import           Data.Function (on)
import           Data.List (sortBy)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (formatTime)
import           Data.Time.Zones (TZ, utcToLocalTimeTZ, utcTZ)
import           System.Locale (defaultTimeLocale)
import           Yesod.Auth
import           Yesod.Form.Bootstrap3

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Format as TF
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Read as T

import           Episodes.DB (updateShowSubscriptionCount)
import qualified TVRage as TVR


-- Show searching parameters.
data SearchShow = SearchShow
    { searchShowText :: Text }


data AddTVRageShow = AddTVRageShow
    { addTVRageShowTvRageId :: Integer }


bootstrapFormLayout :: BootstrapFormLayout
bootstrapFormLayout = BootstrapHorizontalForm labelOffset labelSize inputOffset inputSize
    where
        labelOffset = ColSm 0
        labelSize = ColSm 2
        inputOffset = ColSm 0
        inputSize = ColSm 6


textFieldSettings :: Text -> Text -> FieldSettings site
textFieldSettings labelText placeholderText =
    withPlaceholder placeholderText $ bfs labelText


searchShowForm :: Html -> MForm Handler (FormResult SearchShow, Widget)
searchShowForm = renderBootstrap3 bootstrapFormLayout $ SearchShow
    <$> areq textField (textFieldSettings "Title" "Enter Title") Nothing
    <* bootstrapSubmit (BootstrapSubmit (T.pack "Search") "btn btn-default" [])


addTVRageShowForm :: Html -> MForm Handler (FormResult AddTVRageShow, Widget)
addTVRageShowForm  = renderBootstrap3 bootstrapFormLayout $ AddTVRageShow
    <$> areq intField "TV Rage Id" Nothing


getShowsR :: Handler Html
getShowsR = do
    showEntities :: [Entity Show] <- runDB $ selectList [] [Asc ShowTitle]
    mAuthId <- maybeAuthId
    subscriptions <- case mAuthId of
        Nothing -> return []
        Just authId -> runDB $ selectList [SubscriptionUser ==. authId] []
    let subscribedShowKeys = S.fromList $ map (\s -> subscriptionShow $ entityVal s) subscriptions
    defaultLayout $ do
        setTitle "Shows"
        $(widgetFile "shows")


getEpisodesBySeason :: M.Map SeasonId [Episode] -> SeasonId -> [Episode]
getEpisodesBySeason episodesBySeasonMap seasonId = M.findWithDefault [] seasonId episodesBySeasonMap


groupEpisodesBySeason :: [Episode] -> M.Map SeasonId [Episode]
groupEpisodesBySeason episodeList = sortEpisodes $ foldr _add M.empty episodeList
    where
        _add episode m = M.insert seasonId newSeasonEpisodes m
            where
                seasonId = episodeSeason episode
                newSeasonEpisodes = episode:oldSeasonEpisodes
                oldSeasonEpisodes = M.findWithDefault [] seasonId m
        sortEpisodes = M.map (sortBy episodeSortBy)
        episodeSortBy = compare `on` episodeNumber


formatEpisodeCode :: Int -> Int -> Text
formatEpisodeCode season episode = TL.toStrict $ TF.format "s{}e{}" [TF.left 2 '0' season, TF.left 2 '0' episode]


formatInTimeZone :: TZ -> UTCTime -> Text
formatInTimeZone tz t = formatToText lt
    where
        lt = utcToLocalTimeTZ tz t
        formatToText = T.pack . (formatTime defaultTimeLocale fmt)
        fmt = "%Y-%m-%d %H:%M:%S"


getShowDetailsR :: ShowId -> Handler Html
getShowDetailsR showId = do
    app <- getYesod
    mai <- maybeAuthId
    show_ <- runDB $ get404 showId

    -- todo - make utility function or better yet automate this
    tz <- case mai of
        Just authId -> do
            meProfile <- runDB $ getBy $ UniqueProfileUser authId
            case meProfile of
                Just (Entity _ profile) -> do
                    let tzName = profileTimezone profile
                    let mtz = M.lookup tzName (commonTimeZoneMap app)
                    case mtz of
                        Just _tz -> return _tz
                        Nothing -> return utcTZ
                _ -> return utcTZ
        Nothing -> return utcTZ

    -- liftIO $ putStrLn $ "time zone offset string" ++ timeZoneOffsetString tz

    showSeasons :: [Entity Season] <- runDB $ selectList [SeasonShow ==. showId] [Asc SeasonNumber]
    let showSeasonKeys = map (\s -> entityKey s) showSeasons
    showEpisodes :: [Entity Episode] <- runDB $ selectList [EpisodeSeason <-. showSeasonKeys] []
    let episodesBySeasonMap = groupEpisodesBySeason (map entityVal showEpisodes)
    let episodesBySeason = getEpisodesBySeason episodesBySeasonMap
    let formatInUserTimeZone = formatInTimeZone tz
    defaultLayout $ do
        setTitle "Show Details"
        $(widgetFile "show")


searchShows :: Text -> IO [TVR.Show]
searchShows = TVR.searchShows


getAddShowR :: Handler Html
getAddShowR = do
    _ <- requireAuthId
    (formWidget, formEnctype) <- generateFormPost searchShowForm
    defaultLayout $ do
        $(widgetFile "add-show-form")


postAddShowR :: Handler Html
postAddShowR = do
    ((formResult, formWidget), formEnctype) <- runFormPost searchShowForm
    case formResult of
        FormSuccess searchShow -> do
            let query = searchShowText searchShow :: Text
            showSearchResult <- liftIO $ searchShows query
            let shows_ = map (\s -> (TVR.showTitle s, TVR.showTVRageId s)) showSearchResult
            defaultLayout $ do
                setTitle "Add Show"
                $(widgetFile "add-show-list")
        _ -> do
            defaultLayout $ do
                setTitle "Add Show"
                $(widgetFile "add-show-form")


-- I'm pretty sure it should be somehow fmapped…
maybeTextToMaybeInt :: Maybe Text -> Maybe Int
maybeTextToMaybeInt mt = case mt of
    Nothing -> Nothing
    Just t -> case T.decimal t of
        Left _ -> Nothing
        Right (i, _) -> Just i


-- Convert list of TV Rage episodes and list of season ids to list of pairs of season id and Episode
extractEpisodesForInsert :: TVR.FullShowInfo -> [SeasonId] -> [Episode]
extractEpisodesForInsert fullShowInfo seasonIds =
    concat seasonsEpisodes
    where
        seasonsEpisodes = zipWith extractSeasonEpisodes seasonIds (TVR.fullShowInfoSeasons fullShowInfo)
        extractSeasonEpisodes seasonId tvrSeason = map (convertEpisode seasonId) (TVR.seasonEpisodes tvrSeason)
        convertEpisode seasonId tvrEpisode = Episode { episodeSeason = seasonId
                                                     , episodeNumber = fromInteger $ TVR.episodeNumber tvrEpisode
                                                     , episodeTitle = TVR.episodeTitle tvrEpisode
                                                     , episodeAirDateTime = TVR.episodeAirDateTime tvrEpisode
                                                     , episodeViewCount = 0 }


-- Insert show to DB.
insertShow :: TVR.FullShowInfo -> Handler (Key Show)
insertShow fullShowInfo = do
    let _show = Show { showTitle = TVR.fullShowInfoTitle fullShowInfo
                     , showTvRageId = Just $ fromInteger $ TVR.fullShowInfoTVRageId fullShowInfo
                     , showSubscriptionCount = 0 }
    _showId <- runDB $ insert _show
    let _seasons = map (tvrSeasonToSeason _showId) (TVR.fullShowInfoSeasons fullShowInfo)
    seasonIds <- runDB $ mapM insert _seasons
    let _episodes = extractEpisodesForInsert fullShowInfo seasonIds
    _ <- runDB $ mapM insert _episodes
    return _showId
    where
        tvrSeasonToSeason _showId ts = Season { seasonNumber = fromInteger $ TVR.seasonNumber ts
                                              , seasonShow = _showId}


postAddTVRShowR :: Handler Html
postAddTVRShowR = do
    maybeTVRageIdText <- lookupPostParam "tvRageId"
    let maybeTVRageId = maybeTextToMaybeInt maybeTVRageIdText

    case maybeTVRageId of
        Just tvRageId -> do
            -- get full show info from TV Rage
            maybeShowInfo <- liftIO $ TVR.getFullShowInfo $ toInteger tvRageId
            case maybeShowInfo of
                Just showInfo -> do
                    id_ <- insertShow showInfo
                    redirect $ ShowDetailsR id_
                Nothing -> redirect ShowsR
        _ -> do
            $(logDebug) "invalid form"
            redirect ShowsR


-- User clicks "subscribe" next to show title.
getSubscribeShowR :: ShowId -> Handler Html
getSubscribeShowR showId = do
    authId <- requireAuthId
    mSubscription <- runDB $ getBy $ UniqueSubscriptionUserShow authId showId
    case mSubscription of
        Just _ -> return ()
        Nothing -> runDB $ insert_ Subscription { subscriptionUser = authId, subscriptionShow = showId }
    _ <- runDB $ updateShowSubscriptionCount showId 1
    redirect ShowsR


getUnsubscribeShowR :: ShowId -> Handler Html
getUnsubscribeShowR showId = do
    authId <- requireAuthId
    mSubscription <- runDB $ getBy $ UniqueSubscriptionUserShow authId showId
    case mSubscription of
        Just (Entity subscriptionId _) -> runDB $ delete subscriptionId
        Nothing -> return ()
    _ <- runDB $ updateShowSubscriptionCount showId (-1)
    redirect ShowsR

