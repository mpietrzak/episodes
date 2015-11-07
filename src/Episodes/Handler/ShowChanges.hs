{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Episodes.Handler.ShowChanges (
    getShowChangesAddEpisodesR,
    getShowChangesAddSeasonsR,
    getShowChangesDeleteEpisodeR,
    getShowChangesDeleteSeasonR,
    getShowChangesEditEpisodeR,
    getShowChangesEditSeasonR,
    getShowSubmitChangesR,
    postShowChangesAddEpisodesR,
    postShowChangesAddSeasonsR,
    postShowChangesDeleteEpisodeR,
    postShowChangesDeleteSeasonR,
    postShowChangesEditEpisodeR,
    postShowChangesEditSeasonR
) where

import Prelude hiding (Show, show)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Function (on)
import Data.List (groupBy, sortOn)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Read (decimal)
import Data.Time (getCurrentTime)
import Formatting (sformat, int)
import qualified Data.Map.Strict as M

import Database.Persist (Entity(Entity), entityKey, entityVal)
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Text.Blaze (text)
import Yesod (
    Html,
    defaultLayout,
    get404,
    logDebug,
    lookupSession,
    notFound,
    redirect,
    runDB,
    setSession,
    setTitle )
import  Yesod.Auth (
    -- requireAuthId,
    requireAuthPair )
import Yesod.Form (
    FormResult (FormSuccess),
    MForm,
    aopt,
    areq,
    intField,
    generateFormPost,
    runFormPost,
    textField )
import Yesod.Form.Bootstrap3 (
    BootstrapFormLayout (BootstrapHorizontalForm),
    BootstrapGridOptions (ColSm),
    BootstrapSubmit (BootstrapSubmit),
    bootstrapSubmit,
    renderBootstrap3 )

import Episodes.Common (formatTime)
import Foundation
import Model (
    Account,
    AccountId,
    Episode,
    Season,
    SeasonId,
    Show,
    ShowChangeAddSeasons ( showChangeAddSeasonsCount ),
    ShowChangeId,
    ShowId,
    episodeAirDateTime,
    episodeNumber,
    episodeSeason,
    episodeTitle,
    seasonNumber,
    showTitle)
import Settings (widgetFile)
import qualified Episodes.DB as DB
import qualified Episodes.DB.ShowChanges as DBC
import qualified Episodes.Permissions as P


data AddSeasonsFormData = AddSeasonsFormData { asCount :: Int }

data AddEpisodesFormData = AddEpisodesFormData { aeCount :: Int }

data DeleteEpisodeFormData = DeleteEpisodeFormData

data DeleteSeasonFormData = DeleteSeasonFormData

data EditEpisodeFormData = EditEpisodeFormData { eeTitle :: Maybe Text
                                               , eeNumber :: Maybe Int }

data EditSeasonFormData = EditSeasonFormData { esNumber :: Int }


bootstrapFormLayout :: BootstrapFormLayout
bootstrapFormLayout = BootstrapHorizontalForm labelOffset labelSize inputOffset inputSize
    where
        labelOffset = ColSm 0
        labelSize = ColSm 2
        inputOffset = ColSm 0
        inputSize = ColSm 7


addSeasonsForm :: Html -> MForm Handler (FormResult AddSeasonsFormData, Widget)
addSeasonsForm = renderBootstrap3 bootstrapFormLayout $ AddSeasonsFormData
    <$> areq intField "How many?" Nothing
    <* bootstrapSubmit (BootstrapSubmit ("Add" :: Text) "btn btn-default" [])


addEpisodesForm :: Html -> MForm Handler (FormResult AddEpisodesFormData, Widget)
addEpisodesForm = renderBootstrap3 bootstrapFormLayout $ AddEpisodesFormData
    <$> areq intField "How many?" Nothing
    <* bootstrapSubmit (BootstrapSubmit ("Add" :: Text) "btn btn-default" [])


deleteEpisodeForm :: Html -> MForm Handler (FormResult DeleteEpisodeFormData, Widget)
deleteEpisodeForm = renderBootstrap3 bootstrapFormLayout $ pure DeleteEpisodeFormData
    <* bootstrapSubmit (BootstrapSubmit ("Delete" :: Text) "btn btn-default" [])


deleteSeasonForm :: Html -> MForm Handler (FormResult DeleteSeasonFormData, Widget)
deleteSeasonForm = renderBootstrap3 bootstrapFormLayout $ pure DeleteSeasonFormData
    <* bootstrapSubmit (BootstrapSubmit ("Delete" :: Text) "btn btn-default" [])


editEpisodeForm :: Html -> MForm Handler (FormResult EditEpisodeFormData, Widget)
editEpisodeForm = renderBootstrap3 bootstrapFormLayout $ EditEpisodeFormData
    <$> aopt textField "Title" Nothing
    <*> aopt intField "Number" Nothing
    <* bootstrapSubmit (BootstrapSubmit ("Save" :: Text) "btn btn-default" [])


editSeasonForm :: Html -> MForm Handler (FormResult EditSeasonFormData, Widget)
editSeasonForm = renderBootstrap3 bootstrapFormLayout $ EditSeasonFormData
    <$> areq intField "Season Number" Nothing
    <* bootstrapSubmit (BootstrapSubmit ("Save" :: Text) "btn btn-default" [])


checkCanSubmitChanges :: ShowId -> Handler (AccountId, Account, Show)
checkCanSubmitChanges _showId = do
    (_accountId, _account) <- requireAuthPair
    _show <- runDB $ get404 _showId
    unless (P.canSubmitShowChanges _accountId _account _show) $ do
        $(logDebug) "can't submit changes"
        notFound
    return (_accountId, _account, _show)


getShowData :: ShowId -> Handler ([Entity Season], [Entity Episode], M.Map SeasonId [Entity Episode])
getShowData _showId = do
        seasonEntities <- runDB $ DB.getShowSeasons _showId
        episodeEntities <- runDB $ DB.getShowEpisodes _showId
        let seasons = map entityVal seasonEntities
        let seasonEpisodesMap = groupEpisodes episodeEntities
        return (seasonEntities, episodeEntities, seasonEpisodesMap)
    where
        groupEpisodes eps = M.fromList $ map (\eee -> (episodeSeason $ entityVal $ head eee, eee)) grouped
            where
                sortedEps = sortOn (episodeSeason . entityVal) eps
                groupByFn = (==) `on` (episodeSeason . entityVal)
                (grouped :: [[Entity Episode]]) = groupBy groupByFn sortedEps


get404Season :: ShowId -> Int -> Handler (Entity Season)
get404Season _showId _seasonNumber = do
    mseason <- runDB $ DB.getSeasonByShowAndSeasonNumber _showId _seasonNumber
    case mseason of
        Just eseason -> return eseason
        _ -> notFound


get404Episode :: ShowId -> Int -> Int -> Handler (Entity Episode)
get404Episode _showId _seasonNumber _episodeNumber = do
    _mepisode <- runDB $ DB.getEpisodeByShowAndCode _showId (_seasonNumber, _episodeNumber)
    case _mepisode of
        Just ee -> return ee
        _ -> notFound


getShowChangeId :: AccountId -> ShowId -> Handler ShowChangeId
getShowChangeId _accountId _showId = do
    mChangeIdText <- lookupSession "change-id"
    _mSessChangeId <- case mChangeIdText of
        Just changeIdText ->
            case decimal changeIdText of
                Right (_changeId, _) -> return $ Just $ toSqlKey _changeId
                _ -> return Nothing
        _ -> return Nothing
    _changeId <- case _mSessChangeId of
        Just _i -> return _i
        Nothing -> do
            _now <- liftIO getCurrentTime
            runDB $ DBC.createShowChange _now _accountId _showId
    setSession "change-id" (sformat int (fromSqlKey _changeId))
    return _changeId


getShowSubmitChangesR :: ShowId -> Handler Html
getShowSubmitChangesR _showId = do
    (_accId, _, _show) <- checkCanSubmitChanges _showId
    _changeId <- getShowChangeId _accId _showId
    addSeasonsChangesEntities <- runDB $ DBC.getAddSeasonChanges _changeId
    let addSeasonsChanges = map entityVal addSeasonsChangesEntities
    (seasonEntities, episodeEntities, seasonEpisodesMap) <- getShowData _showId
    defaultLayout $ do
        setTitle $ text $ "Episodes: Submit Show Changes: " <> showTitle _show
        $(widgetFile "changes/show-changes")


getShowChangesAddSeasonsR :: ShowId -> Handler Html
getShowChangesAddSeasonsR _showId = do
    (_, _, _show) <- checkCanSubmitChanges _showId
    (addSeasonsFormWidget, addSeasonsFormEnctype) <- generateFormPost addSeasonsForm
    defaultLayout $ do
        setTitle $ text $ "Episodes: Submit Show Changes: " <> showTitle _show <> ": Add Seasons"
        $(widgetFile "changes/add-seasons")


postShowChangesAddSeasonsR :: ShowId -> Handler Html
postShowChangesAddSeasonsR _showId = do
        (_accId, _, _show) <- checkCanSubmitChanges _showId
        (seasonEntities, episodeEntities, seasonEpisodesMap) <- getShowData _showId
        ((addSeasonsFormResult, addSeasonsFormWidget), addSeasonsFormEnctype) <- runFormPost addSeasonsForm
        case addSeasonsFormResult of
            FormSuccess _f -> do
                _now <- liftIO getCurrentTime
                _showChangeId <- getShowChangeId _accId _showId
                runDB $ DBC.addShowChangeAddSeasons _now _showChangeId (asCount _f)
                redirect $ ShowSubmitChangesR _showId
            _ -> defaultLayout $ do
                setTitle $ text $ "Episodes: Submit Show Changes: " <> showTitle _show
                $(widgetFile "changes/add-seasons")


getShowChangesEditSeasonR :: ShowId -> Int -> Handler Html
getShowChangesEditSeasonR _showId _seasonNumber = do
    (_, _, _show) <- checkCanSubmitChanges _showId
    (editSeasonFormWidget, editSeasonFormEnctype) <- generateFormPost editSeasonForm
    defaultLayout $ do
        setTitle $ text $ "Episodes: Submit Show Changes: " <> showTitle _show <> ": Edit Season"
        $(widgetFile "changes/edit-season")


postShowChangesEditSeasonR :: ShowId -> Int -> Handler Html
postShowChangesEditSeasonR _showId _seasonNumber = do
    (_accId, _, _show) <- checkCanSubmitChanges _showId
    ((editSeasonFormResult, editSeasonFormWidget), editSeasonFormEnctype) <- runFormPost editSeasonForm
    case editSeasonFormResult of
        FormSuccess _f -> do
            _now <- liftIO getCurrentTime
            _showChangeId <- getShowChangeId _accId _showId
            runDB $ DBC.addShowChangeEditSeason _now _showChangeId (esNumber _f)
            redirect $ ShowSubmitChangesR _showId
        _ -> defaultLayout $ do
            setTitle $ text $ "Episodes: Submit Show Changes: " <> showTitle _show <> ": Edit Season"
            $(widgetFile "changes/edit-season")


getShowChangesDeleteSeasonR :: ShowId -> Int -> Handler Html
getShowChangesDeleteSeasonR _showId _seasonNumber = do
    (_, _, _show) <- checkCanSubmitChanges _showId
    (deleteSeasonFormWidget, deleteSeasonFormEnctype) <- generateFormPost deleteSeasonForm
    defaultLayout $ do
        setTitle $ text $ "Episodes: Submit Show Changes: " <> showTitle _show <> ": Delete Season"
        $(widgetFile "changes/delete-season")


postShowChangesDeleteSeasonR :: ShowId -> Int -> Handler Html
postShowChangesDeleteSeasonR _showId _seasonNumber = do
    (_accId, _, _show) <- checkCanSubmitChanges _showId
    Entity _seasonId _season <- get404Season _showId _seasonNumber
    ((deleteSeasonFormResult, deleteSeasonFormWidget), deleteSeasonFormEnctype) <- runFormPost deleteSeasonForm
    case deleteSeasonFormResult of
        FormSuccess _ -> do
            _now <- liftIO getCurrentTime
            _showChangeId <- getShowChangeId _accId _showId
            runDB $ DBC.addShowChangeDeleteSeason _now _showChangeId _seasonId
            redirect $ ShowSubmitChangesR _showId
        _ -> defaultLayout $ do
            setTitle $ text $ "Episodes: Submit Show Changes: " <> showTitle _show <> ": Delete Season"
            $(widgetFile "changes/delete-season")


getShowChangesAddEpisodesR :: ShowId -> Int -> Handler Html
getShowChangesAddEpisodesR _showId _seasonNumber = do
    (_, _, _show) <- checkCanSubmitChanges _showId
    (seasonEntities, episodeEntities, seasonEpisodesMap) <- getShowData _showId
    (addEpisodesFormWidget, addEpisodesFormEnctype) <- generateFormPost addEpisodesForm
    defaultLayout $ do
        setTitle $ text $ "Episodes: Submit Show Changes: " <> showTitle _show
        $(widgetFile "changes/add-episodes")


postShowChangesAddEpisodesR :: ShowId -> Int -> Handler Html
postShowChangesAddEpisodesR _showId _seasonNumber = do
    (_, _, _show) <- checkCanSubmitChanges _showId
    (seasonEntities, episodeEntities, seasonEpisodesMap) <- getShowData _showId
    ((addEpisodesFormResult, addEpisodesFormWidget), addEpisodesFormEnctype) <- runFormPost addEpisodesForm
    defaultLayout $ do
        setTitle $ text $ "Episodes: Submit Show Changes: " <> showTitle _show
        $(widgetFile "changes/add-episodes")


getShowChangesDeleteEpisodeR :: ShowId -> Int -> Int -> Handler Html
getShowChangesDeleteEpisodeR _showId _seasonNumber _episodeNumber = do
    (_, _, _show) <- checkCanSubmitChanges _showId
    _episodeEntity <- get404Episode _showId _seasonNumber _episodeNumber
    let _episode = entityVal _episodeEntity
    (deleteEpisodeFormWidget, deleteEpisodeFormEnctype) <- generateFormPost deleteEpisodeForm
    defaultLayout $ do
        setTitle $ text $ "Episodes: Submit Show Changes: Delete Episode: " <> episodeTitle _episode
        $(widgetFile "changes/delete-episode")


postShowChangesDeleteEpisodeR :: ShowId -> Int -> Int -> Handler Html
postShowChangesDeleteEpisodeR _showId _seasonNumber _episodeNumber = do
    (_accountId, _, _show) <- checkCanSubmitChanges _showId
    _episodeEntity <- get404Episode _showId _seasonNumber _episodeNumber
    let _episode = entityVal _episodeEntity
    let _episodeId = entityKey _episodeEntity
    ((deleteEpisodeFormResult, deleteEpisodeFormWidget), deleteEpisodeFormEnctype) <- runFormPost deleteEpisodeForm
    case deleteEpisodeFormResult of
        FormSuccess _ -> do
            _t <- liftIO getCurrentTime
            _showChangeId <- getShowChangeId _accountId _showId
            runDB $ DBC.addShowChangeDeleteEpisode _t _showChangeId _episodeId
            redirect $ ShowSubmitChangesR _showId
        _ -> defaultLayout $ do
            setTitle $ text $ "Episodes: Submit Show Changes: Delete Episode: " <> episodeTitle _episode
            $(widgetFile "changes/delete-episode")


getShowChangesEditEpisodeR :: ShowId -> Int -> Int -> Handler Html
getShowChangesEditEpisodeR _showId _seasonNumber _episodeNumber = do
    (_, _, _show) <- checkCanSubmitChanges _showId
    -- (seasonEntities, episodeEntities, seasonEpisodesMap) <- getShowData _showId
    _seasonEntity <- get404Season _showId _seasonNumber
    _episodeEntity <- get404Episode _showId _seasonNumber _episodeNumber
    let _season = entityVal _seasonEntity
    let _episode = entityVal _episodeEntity
    (editEpisodeFormWidget, editEpisodeFormEnctype) <- generateFormPost editEpisodeForm
    defaultLayout $ do
        setTitle $ text $ "Episodes: Submit Show Changes: " <> showTitle _show <> ": Edit Episode: " <> episodeTitle _episode
        $(widgetFile "changes/edit-episode")


postShowChangesEditEpisodeR :: ShowId -> Int -> Int -> Handler Html
postShowChangesEditEpisodeR _showId _seasonNumber _episodeNumber = do
    (_accId, _, _show) <- checkCanSubmitChanges _showId
    ((editEpisodeFormResult, editEpisodeFormWidget), editEpisodeFormEnctype) <- runFormPost editEpisodeForm
    _episodeEntity <- get404Episode _showId _seasonNumber _episodeNumber
    _season <- entityVal <$> get404Season _showId _seasonNumber
    let _episode = entityVal _episodeEntity
    let _episodeId = entityKey _episodeEntity
    case editEpisodeFormResult of
        FormSuccess editEpisodeFormData -> do
            _now <- liftIO getCurrentTime
            _showChangeId <- getShowChangeId _accId _showId
            runDB $ DBC.addShowChangeEditEpisode
                _now
                _showChangeId
                _episodeId
                (eeTitle editEpisodeFormData)
                (eeNumber editEpisodeFormData)
            redirect $ ShowSubmitChangesR _showId
        _ -> defaultLayout $ do
            setTitle $ text $ "Episodes: Submit Show Changes: " <> showTitle _show <> ": Edit Episode: " <> episodeTitle _episode
            $(widgetFile "changes/edit-episode")
