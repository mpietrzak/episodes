{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Episodes.Handler.ShowChanges (
    getShowChangesDeleteEpisodeR,
    getShowChangesDeleteSeasonR,
    getShowChangesEditEpisodeR,
    getShowChangesR,
    getShowChangesReviewR,
    postShowChangesAddEpisodeR,
    postShowChangesDeleteEpisodeR,
    postShowChangesDeleteSeasonR,
    postShowChangesEditEpisodeR,
    postShowChangesR
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
    requireAuthId,
    requireAuthPair )
import Yesod.Form (
    FormResult (FormSuccess),
    MForm,
    aopt,
    areq,
    intField,
    generateFormPost,
    renderDivs,
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
    Account ( accountNickname
            , accountEmail
            , accountViews ),
    AccountId,
    Episode,
    Season,
    SeasonId,
    Show,
    ShowChange,
    ShowChangeId,
    ShowId,
    episodeAirDateTime,
    episodeNumber,
    episodeSeason,
    episodeTitle,
    seasonNumber,
    showChangeCreated,
    showChangeDeleteSeasonSeasonNumber,
    showChangeDeleteEpisodeSeasonNumber,
    showChangeDeleteEpisodeEpisodeNumber,
    showChangeEditEpisodeSeasonNumber,
    showChangeEditEpisodeEpisodeNumber,
    showChangeEditEpisodeTitle,
    showChangeModified,
    showTitle)
import Settings (widgetFile)
import qualified Episodes.DB as DB
import qualified Episodes.DB.ShowChanges as DBC
import qualified Episodes.Permissions as P


data DeleteEpisodeFormData = DeleteEpisodeFormData

data DeleteSeasonFormData = DeleteSeasonFormData

data EditEpisodeFormData = EditEpisodeFormData { eeTitle :: Text
                                               , eeSeasonNumber :: Int
                                               , eeEpisodeNumber :: Int }


data SubmitChangeFormData = SubmitChangeFormData


bootstrapFormLayout :: BootstrapFormLayout
bootstrapFormLayout = BootstrapHorizontalForm labelOffset labelSize inputOffset inputSize
    where
        labelOffset = ColSm 0
        labelSize = ColSm 2
        inputOffset = ColSm 0
        inputSize = ColSm 7


deleteEpisodeForm :: Html -> MForm Handler (FormResult DeleteEpisodeFormData, Widget)
deleteEpisodeForm = renderBootstrap3 bootstrapFormLayout $ pure DeleteEpisodeFormData
    <* bootstrapSubmit (BootstrapSubmit ("Delete" :: Text) "btn btn-default" [])


deleteSeasonForm :: Html -> MForm Handler (FormResult DeleteSeasonFormData, Widget)
deleteSeasonForm = renderBootstrap3 bootstrapFormLayout $ pure DeleteSeasonFormData
    <* bootstrapSubmit (BootstrapSubmit ("Delete" :: Text) "btn btn-default" [])


editEpisodeForm :: Maybe Episode -> Html -> MForm Handler (FormResult EditEpisodeFormData, Widget)
editEpisodeForm _mep = renderBootstrap3 bootstrapFormLayout $ EditEpisodeFormData
    <$> areq textField "Title" Nothing
    <*> areq intField "Season Number" Nothing
    <*> areq intField "Episde Number" Nothing
    <* bootstrapSubmit (BootstrapSubmit ("Save" :: Text) "btn btn-default" [])


submitChangeForm :: Html -> MForm Handler (FormResult SubmitChangeFormData, Widget)
submitChangeForm = renderDivs $ pure SubmitChangeFormData


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


getShowChange :: AccountId -> ShowId -> Handler (Entity ShowChange)
getShowChange _accountId _showId = do
    _mc <- runDB $ DBC.getCurrentShowChange _accountId _showId
    case _mc of
        Just _c -> return _c
        Nothing -> do
            _now <- liftIO getCurrentTime
            runDB $ DBC.addShowChange _now _accountId _showId


getShowChangesR :: ShowId -> Handler Html
getShowChangesR _showId = do
    (_accId, _, _show) <- checkCanSubmitChanges _showId
    _changeEntity <- getShowChange _accId _showId
    let _showChangeId = entityKey _changeEntity
    let _showChange = entityVal _changeEntity
    editEpisodeChangeEntities <- runDB $ DBC.getEditEpisodeChanges _showChangeId
    let editEpisodeChanges = map entityVal editEpisodeChangeEntities
    (seasonEntities, episodeEntities, seasonEpisodesMap) <- getShowData _showId
    (submitChangeFormWidget, submitChangeFormEnctype) <- generateFormPost submitChangeForm
    (addEpisodeFormWidget, addEpisodeFormEnctype) <- generateFormPost $ editEpisodeForm Nothing
    defaultLayout $ do
        setTitle $ text $ "Episodes: Submit Show Changes: " <> showTitle _show
        $(widgetFile "changes/show-changes")


getShowChangesReviewR :: Handler Html
getShowChangesReviewR = do
    _accountId <- requireAuthId
    changesData <- runDB $ DBC.getShowChangesForAccept _accountId 1024
    defaultLayout $ do
        setTitle $ text "Episodes: Review Changes"
        $(widgetFile "changes/review")


postShowChangesR :: ShowId -> Handler Html
postShowChangesR _showId = do
    (_accId, _, _show) <- checkCanSubmitChanges _showId
    (Entity _changeId _change) <- getShowChange _accId _showId
    ((submitChangeFormResult, submitChangeFormWidget), submitChangeFormEnctype) <- runFormPost submitChangeForm
    case submitChangeFormResult of
        FormSuccess _f -> do
            _now <- liftIO getCurrentTime
            runDB $ DBC.submitShowChange _now _changeId
        _ -> return ()
    redirect $ ShowChangesR _showId


postShowChangesAddEpisodeR :: ShowId -> Handler Html
postShowChangesAddEpisodeR _showId = do
    (_accId, _, _show) <- checkCanSubmitChanges _showId
    Entity _changeId _change <- getShowChange _accId _showId
    ((addEpisodeFormResult, addEpisodeFormWidget), addEpisodeFormEnctype) <- runFormPost $ editEpisodeForm Nothing
    case addEpisodeFormResult of
        FormSuccess _f -> do
            _now <- liftIO getCurrentTime
            let _seasonNumber = eeSeasonNumber _f
            let _episodeNumber = eeEpisodeNumber _f
            let _title = eeTitle _f
            runDB $ DBC.addShowChangeEditEpisode _now _changeId _seasonNumber _episodeNumber _title
            redirect $ ShowChangesR _showId
        _ -> defaultLayout $ do
                setTitle $ text $ "Episodes: Submit Show Changes: " <> showTitle _show <> ": Add Episode"
                $(widgetFile "changes/add-episode")


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
            (Entity _showChangeId _) <- getShowChange _accId _showId
            runDB $ DBC.addShowChangeDeleteSeason _now _showChangeId _seasonNumber
            redirect $ ShowChangesR _showId
        _ -> defaultLayout $ do
            setTitle $ text $ "Episodes: Submit Show Changes: " <> showTitle _show <> ": Delete Season"
            $(widgetFile "changes/delete-season")


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
            (Entity _showChangeId _) <- getShowChange _accountId _showId
            runDB $ DBC.addShowChangeDeleteEpisode _t _showChangeId _seasonNumber _episodeNumber
            redirect $ ShowChangesR _showId
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
    (editEpisodeFormWidget, editEpisodeFormEnctype) <- generateFormPost $ editEpisodeForm (Just _episode)
    defaultLayout $ do
        setTitle $ text $ "Episodes: Submit Show Changes: " <> showTitle _show <> ": Edit Episode: " <> episodeTitle _episode
        $(widgetFile "changes/edit-episode")


postShowChangesEditEpisodeR :: ShowId -> Int -> Int -> Handler Html
postShowChangesEditEpisodeR _showId _seasonNumber _episodeNumber = do
    (_accId, _, _show) <- checkCanSubmitChanges _showId
    ((editEpisodeFormResult, editEpisodeFormWidget), editEpisodeFormEnctype) <- runFormPost $ editEpisodeForm Nothing
    _episodeEntity <- get404Episode _showId _seasonNumber _episodeNumber
    _season <- entityVal <$> get404Season _showId _seasonNumber
    let _episode = entityVal _episodeEntity
    let _episodeId = entityKey _episodeEntity
    case editEpisodeFormResult of
        FormSuccess editEpisodeFormData -> do
            _now <- liftIO getCurrentTime
            (Entity _showChangeId _) <- getShowChange _accId _showId
            runDB $ DBC.addShowChangeEditEpisode
                _now
                _showChangeId
                (eeSeasonNumber editEpisodeFormData)
                _episodeNumber
                (eeTitle editEpisodeFormData)
            redirect $ ShowChangesR _showId
        _ -> defaultLayout $ do
            setTitle $ text $ "Episodes: Submit Show Changes: " <> showTitle _show <> ": Edit Episode: " <> episodeTitle _episode
            $(widgetFile "changes/edit-episode")
