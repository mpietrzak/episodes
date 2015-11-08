{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Episodes.Handler.ShowEdit (
    getShowEditAddSeasonsR,
    getShowEditAddEpisodesR,
    getEditEpisodeR,
    getShowEditEditSeasonR,
    getShowEditDeleteEpisodeR,
    getShowEditDeleteSeasonR,
    getShowEditR,
    postShowEditAddSeasonsR,
    postShowEditAddEpisodesR,
    postEditEpisodeR,
    postShowEditEditSeasonR,
    postShowEditDeleteEpisodeR,
    postShowEditDeleteSeasonR
) where


import Prelude hiding (Show, show)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Function (on)
import Data.List (groupBy, sortOn)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist (Entity(..), entityVal)
import Formatting ((%), sformat, shown)
import Text.Blaze (text)
import Yesod (
    HandlerSite,
    Html,
    RenderMessage,
    defaultLayout,
    get404,
    hamlet,
    logDebug,
    notFound,
    redirect,
    runDB,
    setTitle,
    toWidget )
import Yesod.Auth (requireAuthId)
import Yesod.Form (
    FieldSettings,
    FormMessage,
    FormResult (FormSuccess),
    MForm,
    areq,
    generateFormPost,
    intField,
    parseHelper,
    parseHelperGen,
    runFormPost,
    textField )
import Yesod.Form.Bootstrap3
import Yesod.Form.Types (Field(..), Enctype(UrlEncoded))
-- import qualified Data.HashMap.Strict as M
import qualified Data.Map.Strict as M
import qualified Data.Time.Format as DTF

import Episodes.Common (formatTime)
import Foundation
import Model (
    AccountId,
    Episode,
    SeasonId,
    Show,
    ShowId,
    episodeAirDateTime,
    episodeNumber,
    episodeSeason,
    episodeTitle,
    seasonNumber,
    showTitle )
import Settings (widgetFile)
import qualified Data.Text as T
import qualified Episodes.DB as DB
import qualified Episodes.Permissions as P


data AddShowSeasonsFormData = AddShowSeasonsFormData { assCount :: Int }

data AddShowEpisodesFormData = AddShowEpisodesFormData { aseCount :: Int }

data EditEpisodeFormData = EditEpisodeFormData { eeTitle :: Text
                                               , eeNumber :: Int
                                               , eeAirTime :: UTCTime }

data EditShowSeasonFormData = EditShowSeasonFormData { essNumber :: Int }

data DeleteShowEpisodeFormData = DeleteShowEpisodeFormData

data DeleteShowSeasonFormData = DeleteShowSeasonFormData



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


bootstrapFormLayout :: BootstrapFormLayout
bootstrapFormLayout = BootstrapHorizontalForm labelOffset labelSize inputOffset inputSize
    where
        labelOffset = ColSm 0
        labelSize = ColSm 2
        inputOffset = ColSm 0
        inputSize = ColSm 7


textFieldSettings :: Text -> Text -> FieldSettings site
textFieldSettings labelText placeholderText =
    withPlaceholder placeholderText $ bfs labelText


addShowSeasonsForm :: Html -> MForm Handler (FormResult AddShowSeasonsFormData, Widget)
addShowSeasonsForm = renderBootstrap3 bootstrapFormLayout $ AddShowSeasonsFormData
    <$> areq intField "How many?" Nothing
    <* bootstrapSubmit (BootstrapSubmit ("Add" :: Text) "btn btn-default" [])


addShowEpisodesForm :: Html -> MForm Handler (FormResult AddShowEpisodesFormData, Widget)
addShowEpisodesForm = renderBootstrap3 bootstrapFormLayout $ AddShowEpisodesFormData
    <$> areq intField "How many?" Nothing
    <* bootstrapSubmit (BootstrapSubmit ("Add" :: Text) "btn btn-default" [])


editEpisodeForm :: Maybe Episode -> Html -> MForm Handler (FormResult EditEpisodeFormData, Widget)
editEpisodeForm mep = renderBootstrap3 bootstrapFormLayout $ EditEpisodeFormData
        <$> areq textField "Title" (episodeTitle <$> mep)
        <*> areq intField "Episode Number" (episodeNumber <$> mep)
        <*> areq utcTimeField "Air Date/Time" maybeAirTime
        <* bootstrapSubmit (BootstrapSubmit ("Save" :: Text) "btn btn-default" [])
    where
        maybeAirTime = case mep of
            Nothing -> Nothing
            Just ep -> episodeAirDateTime ep


editShowSeasonForm :: Html -> MForm Handler (FormResult EditShowSeasonFormData, Widget)
editShowSeasonForm = renderBootstrap3 bootstrapFormLayout $ EditShowSeasonFormData
    <$> areq intField "Season Number" Nothing
    <* bootstrapSubmit (BootstrapSubmit ("Save" :: Text) "btn btn-default" [])


deleteShowEpisodeForm :: Html -> MForm Handler (FormResult DeleteShowEpisodeFormData, Widget)
deleteShowEpisodeForm = renderBootstrap3 bootstrapFormLayout $ pure DeleteShowEpisodeFormData
    <* bootstrapSubmit (BootstrapSubmit ("Delete" :: Text) "btn btn-default" [])


deleteShowSeasonForm :: Html -> MForm Handler (FormResult DeleteShowSeasonFormData, Widget)
deleteShowSeasonForm = renderBootstrap3 bootstrapFormLayout $ pure DeleteShowSeasonFormData
    <* bootstrapSubmit (BootstrapSubmit ("Delete" :: Text) "btn btn-default" [])


checkCanEdit :: ShowId -> Handler (AccountId, Show)
checkCanEdit sid = do
    a <- requireAuthId
    s <- runDB $ get404 sid
    unless (P.canEditShow a s) notFound
    return (a, s)


getShowEditR :: ShowId -> Handler Html
getShowEditR showId = do
        (_, show) <- checkCanEdit showId
        seasonEntities <- runDB $ DB.getShowSeasons showId
        episodeEntities <- runDB $ DB.getShowEpisodes showId
        let seasons = map entityVal seasonEntities
        let seasonEpisodesMap = groupEpisodes episodeEntities
        defaultLayout $ do
            setTitle $ text $ "Episodes: Show Edit: " <> showTitle show
            $(widgetFile "show-edit")
    where
        groupEpisodes eps = M.fromList $ map (\(eee::[Entity Episode]) -> (episodeSeason $ entityVal $ head eee, eee)) grouped
            where
                sortedEps = sortOn (episodeSeason . entityVal) eps
                groupByFn = (==) `on` (episodeSeason . entityVal)
                (grouped :: [[Entity Episode]]) = groupBy groupByFn sortedEps


getShowEditAddSeasonsR :: ShowId -> Handler Html
getShowEditAddSeasonsR showId = do
    (accountId, show) <- checkCanEdit showId
    (formWidget, formEnctype) <- generateFormPost addShowSeasonsForm
    defaultLayout $ do
        setTitle $ text $ "Episodes: Show Edit: " <> showTitle show <> ": Add Season"
        $(widgetFile "show-edit/add-season")


getShowEditAddEpisodesR :: ShowId -> Int -> Handler Html
getShowEditAddEpisodesR _showId _seasonNumber = do
    (_accountId, _show) <- checkCanEdit _showId
    (formWidget, formEnctype) <- generateFormPost addShowEpisodesForm
    defaultLayout $ do
        setTitle $ text $ "Episodes: Show Edit: " <> showTitle _show <> ": Add Episodes"
        $(widgetFile "show-edit/add-episodes")


getEditEpisodeR :: ShowId -> Int -> Int -> Handler Html
getEditEpisodeR _showId _seasonNumber _episodeNumber = do
    (_accountId, _show) <- checkCanEdit _showId
    _mepisode <- runDB $ DB.getEpisodeByShowAndCode _showId (_seasonNumber, _episodeNumber)
    (formWidget, formEnctype) <- generateFormPost $ editEpisodeForm (entityVal <$> _mepisode)
    defaultLayout $ do
        setTitle $ text $ "Episodes: Show Edit: " <> showTitle _show <> ": Edit Episode"
        $(widgetFile "show-edit/edit-episode")


getShowEditEditSeasonR :: ShowId -> Int -> Handler Html
getShowEditEditSeasonR _showId _seasonNumber = do
    (_accountId, _show) <- checkCanEdit _showId
    (formWidget, formEnctype) <- generateFormPost editShowSeasonForm
    defaultLayout $ do
        setTitle $ text $ "Episodes: Show Edit: " <> showTitle _show <> ": Edit Season"
        $(widgetFile "show-edit/edit-season")


getShowEditDeleteEpisodeR :: ShowId -> Int -> Int -> Handler Html
getShowEditDeleteEpisodeR _showId _seasonNumber _episodeNumber = do
    (_, _show) <- checkCanEdit _showId
    (formWidget, formEnctype) <- generateFormPost deleteShowEpisodeForm
    defaultLayout $ do
        setTitle $ text $ "Episodes: Show Edit: " <> showTitle _show <> ": Delete Episode"
        $(widgetFile "show-edit/delete-episode")


getShowEditDeleteSeasonR :: ShowId -> Int -> Handler Html
getShowEditDeleteSeasonR showId seasonNumber = do
    (_, show) <- checkCanEdit showId
    (formWidget, formEnctype) <- generateFormPost deleteShowSeasonForm
    defaultLayout $ do
        setTitle $ text $ "Episodes: Show Edit: " <> showTitle show <> ": Delete Season"
        $(widgetFile "show-edit/delete-season")


postShowEditAddSeasonsR :: ShowId -> Handler Html
postShowEditAddSeasonsR showId = do
    (accountId, show) <- checkCanEdit showId
    ((formResult, formWidget), formEnctype) <- runFormPost addShowSeasonsForm
    case formResult of
        FormSuccess r -> do
            now <- liftIO getCurrentTime
            runDB $ DB.addShowSeasons now showId (assCount r)
            redirect $ ShowEditR showId
        _ -> getShowEditAddSeasonsR showId


postShowEditAddEpisodesR :: ShowId -> Int -> Handler Html
postShowEditAddEpisodesR _showId _seasonNumber = do
    (_accountId, _show) <- checkCanEdit _showId
    ((formResult, formWidget), formEnctype) <- runFormPost addShowEpisodesForm
    case formResult of
        FormSuccess r -> do
            now <- liftIO getCurrentTime
            runDB $ DB.addShowEpisodes now _showId _seasonNumber (aseCount r)
            redirect $ ShowEditR _showId
        _ -> getShowEditAddEpisodesR _showId _seasonNumber


postEditEpisodeR :: ShowId -> Int -> Int -> Handler Html
postEditEpisodeR _showId _seasonNumber _episodeNumber = do
    (_accountId, _show) <- checkCanEdit _showId
    ((formResult, formWidget), formEnctype) <- runFormPost $ editEpisodeForm Nothing
    case formResult of
        FormSuccess _f -> do
            now <- liftIO getCurrentTime
            runDB $ DB.updateEpisode2
                now
                (_showId, _seasonNumber, _episodeNumber)
                (eeTitle _f)
                (eeNumber _f)
                (eeAirTime _f)
            redirect $ ShowEditR _showId
        _ -> defaultLayout $ do
            setTitle $ text $ "Episodes: Show Edit: " <> showTitle _show <> ": Edit Episode"
            $(widgetFile "show-edit/edit-episode")



postShowEditEditSeasonR :: ShowId -> Int -> Handler Html
postShowEditEditSeasonR _showId _seasonNumber = do
    (_accountId, _show) <- checkCanEdit _showId
    ((formResult, formWidget), formEnctype) <- runFormPost editShowSeasonForm
    case formResult of
        FormSuccess r -> do
            now <- liftIO getCurrentTime
            runDB $ DB.updateSeason now _showId _seasonNumber (essNumber r)
            redirect $ ShowEditR _showId
        _ -> getShowEditEditSeasonR _showId _seasonNumber


postShowEditDeleteEpisodeR :: ShowId -> Int -> Int -> Handler Html
postShowEditDeleteEpisodeR _showId _seasonNumber _episodeNumber = do
    (_accountId, _show) <- checkCanEdit _showId
    ((formResult, formWidget), formEnctype) <- runFormPost deleteShowEpisodeForm
    case formResult of
        FormSuccess _ -> do
            $(logDebug) $ sformat
                            ("deleting episode: show: " % shown % ", season number: " % shown % ", episode number: " % shown)
                            _showId
                            _seasonNumber
                            _episodeNumber
            runDB $ DB.deleteEpisodeByEpisodeCode _showId (_seasonNumber, _episodeNumber)
            redirect $ ShowEditR _showId
        _ -> getShowEditDeleteEpisodeR _showId _seasonNumber _episodeNumber


postShowEditDeleteSeasonR :: ShowId -> Int -> Handler Html
postShowEditDeleteSeasonR _showId _seasonNumber = do
    (accountId, show) <- checkCanEdit _showId
    ((formResult, formWidget), formEnctype) <- runFormPost deleteShowSeasonForm
    case formResult of
        FormSuccess _ -> do
            runDB $ DB.deleteShowSeason _showId _seasonNumber
            redirect $ ShowEditR _showId
        _ -> getShowEditDeleteSeasonR _showId _seasonNumber

