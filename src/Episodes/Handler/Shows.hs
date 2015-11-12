{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Episodes.Handler.Shows where


import           Prelude hiding (show, Show)
import           Data.Bool (bool)
import           Data.Function (on)
import           Data.List (sortBy)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
-- import           Data.Time.Zones (utcTZ)
import           Database.Persist.Sql (fromSqlKey)
import           Text.Blaze (text)
import           Yesod
import           Yesod.Auth
import           Yesod.Form.Bootstrap3

-- import           Yesod.PureScript (addPureScriptWidget)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Read as T

import           Foundation
import           Episodes.Common (choose,
                                  forceLazyText,
                                  forceText, formatEpisodeCode, formatInTimeZone, formatTime,
                                  getUserEpisodeLinks, getUserTimeZone)
import           Episodes.DB (addPrivateShow,
                              getEpisodeStatusesByShowAndUser,
                              getPopularShows,
                              getShowEpisodes,
                              getShowSeasons,
                              getShowSeasonCollapse,
                              getUserShowsEpisodesLastSeen,
                              setSubscriptionStatus,
                              updateShowSubscriptionCount)

import           Episodes.StaticFiles (js_episodes_js)
import           Model
import           Settings (widgetFile)
import qualified Episodes.Permissions as P
import qualified TVRage as TVR


-- Show searching parameters.
data SearchShow = SearchShow
    { searchShowText :: Text }


data AddTVRageShow = AddTVRageShow
    { addTVRageShowTvRageId :: Integer }


data AddShowFormData = AddShowFormData { asShowTitle :: Text }


bootstrapFormLayout :: BootstrapFormLayout
bootstrapFormLayout = BootstrapHorizontalForm labelOffset labelSize inputOffset inputSize
    where
        labelOffset = ColSm 0
        labelSize = ColSm 1
        inputOffset = ColSm 0
        inputSize = ColSm 7


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


addShowForm :: Html -> MForm Handler (FormResult AddShowFormData, Widget)
addShowForm = renderBootstrap3 bootstrapFormLayout $ AddShowFormData <$> areq textField (textFieldSettings "Title" "Enter Show Title") Nothing


getShowsR :: Handler Html
getShowsR = do
    showEntities <- runDB $ selectList [ShowPublic ==. True] [Asc ShowTitle] -- TODO via API
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
        addScript $ StaticR js_episodes_js
        -- toWidget [julius|PS["Episodes.ShowSubscriptions"].main()|]
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
    now <- liftIO $ getCurrentTime
    mai <- maybeAuthId
    ma <- maybeAuth
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
    let getEpisodeStatusByEpisodeId = flip (M.findWithDefault "unseen") episodeStatusByEpisodeId

    let formatInUserTimeZone = formatInTimeZone tz

    episodeLinks <- getUserEpisodeLinks mai

    seasonCollapse <- case mai of
        Just i -> runDB $ getShowSeasonCollapse i showId
        _ -> return $ const False

    -- this is only for link
    let minAccEditAgeSec = 60 * 60 * 24 * 60
    let canEdit = case ma of
            Just (Entity _aid _) -> P.canEditShow _aid show
            Nothing -> False
    let canSubmitChanges = case ma of
            Just (Entity _ _a) -> accountAdmin _a  -- not canEdit && accountCreated _a < addUTCTime (-minAccEditAgeSec) now
            Nothing -> False

    defaultLayout $ do
        setTitle $ text $ "Episodes: " <> showTitle show
        addScript $ StaticR js_episodes_js
        toWidget [julius|PS["Episodes"].main()|]
        $(widgetFile "show")


searchShows :: Text -> IO [TVR.Show]
searchShows = TVR.searchShows


getAddShowR :: Handler Html
getAddShowR = do
    _ <- requireAuthId
    (formWidget, formEnctype) <- generateFormPost addShowForm
    defaultLayout $ do
        $(widgetFile "add-show-form")


-- postAddShowR :: Handler Html
-- postAddShowR = do
--     ((formResult, formWidget), formEnctype) <- runFormPost searchShowForm
--     case formResult of
--         FormSuccess searchShow -> do
--             let query = searchShowText searchShow :: Text
--             showSearchResult <- liftIO $ searchShows query
--             let shows_ = map (\s -> (TVR.showTitle s, TVR.showTVRageId s)) showSearchResult
--             defaultLayout $ do
--                 setTitle "Add Show"
--                 $(widgetFile "add-show-list")
--         _ -> do
--             defaultLayout $ do
--                 setTitle "Add Show"
--                 $(widgetFile "add-show-form")


postAddShowR :: Handler Html
postAddShowR = do
    accountId <- requireAuthId
    ((formResult, formWidget), formEnctype) <- runFormPost addShowForm
    case formResult of
        FormSuccess addShowFormData -> do
            let _title = asShowTitle addShowFormData
            now <- liftIO $ getCurrentTime
            e <- runDB $ addPrivateShow now accountId _title
            case e of
                Right showId -> redirect $ ShowDetailsR showId
                _ -> error "failed to add show"
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
                                                     , episodeAirDateTime = Just $ TVR.episodeAirDateTime tvrEpisode
                                                     , episodeViewCount = 0
                                                     , episodeModified = now
                                                     , episodeCreated = now }


-- Insert show to DB.
insertTVRageShow :: TVR.FullShowInfo -> Handler (Key Show)
insertTVRageShow fullShowInfo = do
    now <- liftIO getCurrentTime
    let _show = Show { showTitle = TVR.fullShowInfoTitle fullShowInfo
                     , showTvRageId = Just $ fromInteger $ TVR.fullShowInfoTVRageId fullShowInfo
                     , showSubscriptionCount = 0
                     , showSubmitted = True     -- TV Rage shows are submitted by default
                     , showPublic = True        -- TV Rage shows are public by default
                     , showLocal = False        -- TV Rage shows are synchronised, not managed locally
                     , showAddedBy = Nothing    -- TV Rage shows are not owned
                     , showCreated = now
                     , showModified = now
                     , showLastUpdate = Just now
                     , showNextUpdate = Just now }
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
                    id_ <- insertTVRageShow showInfo
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
        $(widgetFile "last-episodes")

