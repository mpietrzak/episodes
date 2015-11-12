{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Episodes.Handler.ShowChanges (
    getShowChangesAcceptR,
    getShowChangesDeleteEpisodeR,
    getShowChangesDeleteSeasonR,
    getShowChangesEditEpisodeR,
    getShowChangesR,
    getShowChangesRejectR,
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
import Data.Time (UTCTime, getCurrentTime)
import Formatting (sformat, int)
import qualified Data.Map.Strict as M
import qualified Data.Time.Format as DTF

import Database.Persist (Entity(Entity), entityKey, entityVal)
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Text.Blaze (text)
import Yesod (
    HandlerSite,
    Html,
    RenderMessage,
    defaultLayout,
    get404,
    hamlet,
    logDebug,
    lookupSession,
    notFound,
    redirect,
    runDB,
    setSession,
    setTitle,
    toWidget )
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
    parseHelperGen,
    renderDivs,
    runFormPost,
    textField )
import Yesod.Form.Bootstrap3 (
    BootstrapFormLayout (BootstrapHorizontalForm),
    BootstrapGridOptions (ColSm),
    BootstrapSubmit (BootstrapSubmit),
    bootstrapSubmit,
    renderBootstrap3 )
import Yesod.Form.Types (Field(..), Enctype(UrlEncoded))
import qualified Data.Text as T

import Episodes.Common (formatTime)
import Foundation
import Model (
    Account ( accountAdmin
            , accountNickname
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
                                               , eeEpisodeNumber :: Int
                                               , eeAirDateTime :: UTCTime }


data SubmitChangeFormData = SubmitChangeFormData


bootstrapFormLayout :: BootstrapFormLayout
bootstrapFormLayout = BootstrapHorizontalForm labelOffset labelSize inputOffset inputSize
    where
        labelOffset = ColSm 0
        labelSize = ColSm 2
        inputOffset = ColSm 0
        inputSize = ColSm 7


utcTimeField :: (Monad m, RenderMessage (HandlerSite m) Text) => Field m UTCTime
utcTimeField = Field {
            fieldParse = parseHelperGen $ \s ->
                case DTF.parseTimeM True DTF.defaultTimeLocale timeFormat (T.unpack s) of
                    Just _t -> Right _t
                    Nothing -> Left ("Invalid date/time" :: Text),
            fieldView = \theId name attrs val isReq -> toWidget
                [hamlet|<input id="#{theId}" name="#{name}" *{attrs} type="text" :isReq:required="" value="#{showTime val}">|],
            fieldEnctype = UrlEncoded
        }
    where
        timeFormat = "%Y-%m-%d %H:%M:%S"
        showTime val = case val of
            Right _t -> formatTime _t
            Left _ -> ""


deleteEpisodeForm :: Html -> MForm Handler (FormResult DeleteEpisodeFormData, Widget)
deleteEpisodeForm = renderBootstrap3 bootstrapFormLayout $ pure DeleteEpisodeFormData
    <* bootstrapSubmit (BootstrapSubmit ("Delete" :: Text) "btn btn-default" [])


deleteSeasonForm :: Html -> MForm Handler (FormResult DeleteSeasonFormData, Widget)
deleteSeasonForm = renderBootstrap3 bootstrapFormLayout $ pure DeleteSeasonFormData
    <* bootstrapSubmit (BootstrapSubmit ("Delete" :: Text) "btn btn-default" [])


editEpisodeForm :: Maybe (Season, Episode) -> Html -> MForm Handler (FormResult EditEpisodeFormData, Widget)
editEpisodeForm _mse = renderBootstrap3 bootstrapFormLayout $ EditEpisodeFormData
        <$> areq textField "Title" _mtitle
        <*> areq intField "Season Number" _mseason
        <*> areq intField "Episde Number" _mepisode
        <*> areq utcTimeField "Air Date/Time" _mairts
        <* bootstrapSubmit (BootstrapSubmit ("Save" :: Text) "btn btn-default" [])
    where
        _mtitle = (episodeTitle . snd) <$> _mse
        _mseason = (seasonNumber . fst) <$> _mse
        _mepisode = (episodeNumber . snd) <$> _mse
        _mairts = maybe Nothing (episodeAirDateTime . snd) _mse

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
    let isEmpty _ds _de _ee = case (_ds, _de, _ee) of
            ([], [], []) -> True
            _ -> False
    defaultLayout $ do
        setTitle $ text "Episodes: Review Changes"
        $(widgetFile "changes/review")


getShowChangesAcceptReject :: Bool -> ShowChangeId -> Handler Html
getShowChangesAcceptReject _accept _showChangeId = do
    (_, _acc) <- requireAuthPair
    unless (accountAdmin _acc) notFound
    _now <- liftIO getCurrentTime
    runDB $ if _accept
        then DBC.acceptShowChange _now _showChangeId
        else DBC.rejectShowChange _now _showChangeId
    redirect ShowChangesReviewR


getShowChangesAcceptR :: ShowChangeId -> Handler Html
getShowChangesAcceptR = getShowChangesAcceptReject True


getShowChangesRejectR :: ShowChangeId -> Handler Html
getShowChangesRejectR = getShowChangesAcceptReject False


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
            let _airdt = eeAirDateTime _f
            runDB $ DBC.addShowChangeEditEpisode _now _changeId _seasonNumber _episodeNumber _title _airdt
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
    (editEpisodeFormWidget, editEpisodeFormEnctype) <- generateFormPost $ editEpisodeForm (Just (_season, _episode))
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
                (eeAirDateTime editEpisodeFormData)
            redirect $ ShowChangesR _showId
        _ -> defaultLayout $ do
            setTitle $ text $ "Episodes: Submit Show Changes: " <> showTitle _show <> ": Edit Episode: " <> episodeTitle _episode
            $(widgetFile "changes/edit-episode")
