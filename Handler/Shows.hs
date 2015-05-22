{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}


module Handler.Shows where


import           Prelude hiding (show, Show)
import           Control.Applicative
import           Control.Concurrent.Async (concurrently)
import           Data.Function (on)
import           Data.List (sortBy)
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Data.Time.Zones (utcTZ)
import           Yesod
import           Yesod.Auth
import           Yesod.Form.Bootstrap3
import           Yesod.PureScript (addPureScriptWidget)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Read as T

import           Foundation
import           Episodes.Common (choose,
                                  forceText, formatEpisodeCode, formatInTimeZone, formatTime,
                                  getUserEpisodeLinks, getUserTimeZone)
import           Episodes.DB (getEpisodeStatusesByShowAndUser,
                              getPopularShows,
                              getShowEpisodes,
                              getShowSeasons,
                              getUserShowsEpisodesLastSeen,
                              setSubscriptionStatus,
                              updateShowSubscriptionCount)
import           Model
import           Settings (widgetFile, yesodPureScriptOptions)
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
    showEntities <- runDB $ selectList [] [Asc ShowTitle]
    mAuthId <- maybeAuthId
    subscribedShowKeys <- case mAuthId of
        Nothing -> do
            popularShows <- runDB $ getPopularShows 32
            return $ S.fromList $ map entityKey popularShows
        Just authId -> do
            subscriptions <- runDB $ selectList [SubscriptionAccount ==. authId] []
            return $ S.fromList $ map (\s -> subscriptionShow $ entityVal s) subscriptions
    defaultLayout $ do
        setTitle "Shows"
        -- addScript $ PureScriptR $ YPS.getPureScriptRoute ["ShowSubscriptions"]
        $(addPureScriptWidget yesodPureScriptOptions "ShowSubscriptions")
        $(widgetFile "shows")


getEpisodesBySeason :: M.Map SeasonId [Entity Episode] -> SeasonId -> [Entity Episode]
getEpisodesBySeason episodesBySeasonMap seasonId = M.findWithDefault [] seasonId episodesBySeasonMap


groupEpisodesBySeason :: [Entity Episode] -> M.Map SeasonId [Entity Episode]
groupEpisodesBySeason episodeList = sortEpisodes $ foldr _add M.empty episodeList
    where
        _add episode m = M.insert seasonId newSeasonEpisodes m
            where
                seasonId = episodeSeason (entityVal episode)
                newSeasonEpisodes = episode:oldSeasonEpisodes
                oldSeasonEpisodes = M.findWithDefault [] seasonId m
        sortEpisodes = M.map (sortBy episodeSortBy)
        episodeSortBy = compare `on` (\ev -> (episodeNumber (entityVal ev), entityKey ev))


getShowDetailsR :: ShowId -> Handler Html
getShowDetailsR showId = do
    app <- getYesod
    mai <- maybeAuthId
    show <- runDB $ get404 showId

    tz <- getUserTimeZone

    showEpisodes <- runDB $ getShowEpisodes showId
    showSeasons <- runDB $ getShowSeasons showId

    let episodesBySeasonMap = groupEpisodesBySeason showEpisodes
    let episodesBySeason = getEpisodesBySeason episodesBySeasonMap

    episodeStatusEntities :: [Entity EpisodeStatus] <- case mai of
        Just authId -> runDB $ getEpisodeStatusesByShowAndUser showId authId
        Nothing -> return []
    let episodeStatusByEpisodeId = M.fromList $ map (\e -> (episodeStatusEpisode (entityVal e), episodeStatusStatus (entityVal e))) episodeStatusEntities
    let getEpisodeStatusByEpisodeId = \eid -> M.findWithDefault "unseen" eid episodeStatusByEpisodeId

    let formatInUserTimeZone = formatInTimeZone tz

    episodeLinks <- getUserEpisodeLinks mai

    defaultLayout $ do
        setTitle "Show Details"
        -- addScript $ PureScriptR $ YPS.getPureScriptRoute ["ShowDetails"]
        $(addPureScriptWidget yesodPureScriptOptions "ShowDetails")
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
extractEpisodesForInsert :: UTCTime -> TVR.FullShowInfo -> [SeasonId] -> [Episode]
extractEpisodesForInsert now fullShowInfo seasonIds =
    concat seasonsEpisodes
    where
        seasonsEpisodes = zipWith extractSeasonEpisodes seasonIds (TVR.fullShowInfoSeasons fullShowInfo)
        extractSeasonEpisodes seasonId tvrSeason = map (convertEpisode seasonId) (TVR.seasonEpisodes tvrSeason)
        convertEpisode seasonId tvrEpisode = Episode { episodeSeason = seasonId
                                                     , episodeNumber = fromInteger $ TVR.episodeNumber tvrEpisode
                                                     , episodeTitle = TVR.episodeTitle tvrEpisode
                                                     , episodeAirDateTime = TVR.episodeAirDateTime tvrEpisode
                                                     , episodeViewCount = 0
                                                     , episodeModified = now
                                                     , episodeCreated = now }


-- Insert show to DB.
insertShow :: TVR.FullShowInfo -> Handler (Key Show)
insertShow fullShowInfo = do
    now <- liftIO getCurrentTime
    let _show = Show { showTitle = TVR.fullShowInfoTitle fullShowInfo
                     , showTvRageId = Just $ fromInteger $ TVR.fullShowInfoTVRageId fullShowInfo
                     , showSubscriptionCount = 0
                     , showCreated = now
                     , showModified = now
                     , showLastUpdate = now
                     , showNextUpdate = now }
    _showId <- runDB $ insert _show
    let _seasons = map (tvrSeasonToSeason now _showId) (TVR.fullShowInfoSeasons fullShowInfo)
    seasonIds <- runDB $ mapM insert _seasons
    let _episodes = extractEpisodesForInsert now fullShowInfo seasonIds
    _ <- runDB $ mapM insert _episodes
    return _showId
    where
        tvrSeasonToSeason now _showId ts = Season { seasonNumber = fromInteger $ TVR.seasonNumber ts
                                                  , seasonShow = _showId
                                                  , seasonCreated = now
                                                  , seasonModified = now }


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
    mSubscription <- runDB $ getBy $ UniqueSubscriptionAccountShow authId showId
    now <- liftIO getCurrentTime
    case mSubscription of
        Just _ -> return ()
        Nothing -> runDB $ setSubscriptionStatus now authId showId True
    _ <- runDB $ updateShowSubscriptionCount showId 1
    redirect ShowsR


getUnsubscribeShowR :: ShowId -> Handler Html
getUnsubscribeShowR showId = do
    authId <- requireAuthId
    mSubscription <- runDB $ getBy $ UniqueSubscriptionAccountShow authId showId
    case mSubscription of
        Just (Entity subscriptionId _) -> runDB $ delete subscriptionId
        Nothing -> return ()
    _ <- runDB $ updateShowSubscriptionCount showId (-1)
    redirect ShowsR


getLastEpisodesR :: Handler Html
getLastEpisodesR = do
    ma <- maybeAuthId
    lastShowEpisodes <- case ma of
            Just _a -> runDB $ getUserShowsEpisodesLastSeen _a
            Nothing -> return []
    defaultLayout $ do
        setTitle "Last Episodes"
        -- $(addPureScriptWidget yesodPureScriptOptions "ShowSubscriptions")
        $(widgetFile "last-episodes")


