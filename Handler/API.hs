{-# LANGUAGE OverloadedStrings #-}

module Handler.API (
    postSetEpisodeStatusR,
    postSetShowSubscriptionStatusR
) where


import Prelude
import Yesod
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Database.Persist
import Database.Persist.Class
import Database.Persist.Sql
import qualified Data.HashMap.Strict as HM
import qualified Network.HTTP.Types as HT
import qualified Yesod.Auth as YA

import Foundation
import Model
import qualified Episodes.DB as DB


postSetEpisodeStatusR :: Handler TypedContent
postSetEpisodeStatusR = do
    accId <- YA.requireAuthId
    setEpisodeStatusRequest <- requireJsonBody
    case setEpisodeStatusRequest of
        Object m -> do
            let mEpisodeId = HM.lookup "episodeId" m
            let mEpisodeStatus = HM.lookup "episodeStatus" m
            case (mEpisodeId, mEpisodeStatus) of
                (Just (Number episodeIdScientific), Just (Bool episodeStatus)) -> do
                    let episodeId = truncate episodeIdScientific
                    let episodeKey = toSqlKey episodeId :: EpisodeId
                    now <- liftIO getCurrentTime
                    runDB $ do
                        DB.updateEpisodeStatus accId episodeKey now episodeStatus
                        DB.updateEpisodeViewCount episodeKey (if episodeStatus then 1 else (-1))
                    selectRep $ do
                        let status = "ok" :: Text
                        provideRep $ return $ object
                            [ "status" .= status ]
                _ -> sendResponseStatus HT.status500 ("Invalid JSON request" :: Text)
        _ -> sendResponseStatus HT.status500 ("Expected JSON request" :: Text)


postSetShowSubscriptionStatusR :: Handler TypedContent
postSetShowSubscriptionStatusR = do
    accId <- YA.requireAuthId
    setShowSubscriptionStatusRequest <- requireJsonBody
    case setShowSubscriptionStatusRequest of
        Object m -> do
            let mShowId = HM.lookup "showId" m
            let mShowSubscriptionStatus = HM.lookup "status" m
            case (mShowId, mShowSubscriptionStatus) of
                (Just (Number showIdSci), Just (Bool status)) -> do
                    let showId = truncate showIdSci
                    let showKey = toSqlKey showId :: ShowId
                    now <- liftIO getCurrentTime
                    runDB $ do
                        DB.updateShowSubscriptionCount showKey (if status then 1 else (-1))
                        DB.setSubscriptionStatus now accId showKey status
                    selectRep $ do
                        let _status = "ok" :: Text
                        provideRep $ return $ object
                            [ "status" .= _status ]
                _ -> sendResponseStatus HT.status500 ("Invalid JSON request" :: Text)
        _ -> sendResponseStatus HT.status500 ("Expected JSON request" :: Text)


