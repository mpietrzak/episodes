{-# LANGUAGE OverloadedStrings #-}

module Episodes.Handler.API (
    postSetEpisodeStatusR,
    postSetSeasonCollapseR,
    postSetShowSubscriptionStatusR
) where


import Prelude
import Yesod
import Control.Monad (mzero)
-- import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Database.Persist.Sql
import qualified Data.HashMap.Strict as HM
import qualified Network.HTTP.Types as HT
import qualified Yesod.Auth as YA

import Foundation
import Model
import qualified Episodes.DB as DB


data SeasonCollapseRequest = SeasonCollapseRequest { scRequestSeason :: Int64
                                                   , scRequestCollapse :: Bool }

data SeasonCollapseResponse = SeasonCollapseResponse { scResponseStatus :: Text }


instance FromJSON SeasonCollapseRequest where
    parseJSON (Object v) = SeasonCollapseRequest <$> v .: "season"
                                                 <*> v .: "collapse"
    parseJSON _ = mzero


instance ToJSON SeasonCollapseResponse where
    toJSON (SeasonCollapseResponse status) = object [ "status" .= status ]


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


postSetSeasonCollapseR :: Handler Value
postSetSeasonCollapseR = do
    accId <- YA.requireAuthId
    scReq <- requireJsonBody
    let collapse = scRequestCollapse scReq
    let season = scRequestSeason scReq
    runDB $ DB.setSeasonCollapse collapse accId (toSqlKey season)
    returnJson $ SeasonCollapseResponse "ok"

