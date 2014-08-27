{-# LANGUAGE OverloadedStrings #-}

module Handler.API (
    postSetEpisodeStatusR
) where


import Prelude
import Yesod
import Data.Text (Text)
import Data.Time (getCurrentTime)
import qualified Data.HashMap.Strict as HM
import qualified Network.HTTP.Types as HT
import qualified Yesod.Auth as YA

import Episodes.DB (updateEpisodeStatus, updateEpisodeViewCount)
import Foundation


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
                    let episodeId = truncate episodeIdScientific -- no point to be strict
                    let episodeKey = Key $ PersistInt64 episodeId
                    now <- liftIO getCurrentTime
                    runDB $ do
                        updateEpisodeStatus accId episodeKey now episodeStatus
                        updateEpisodeViewCount episodeKey (if episodeStatus then 1 else (-1))
                    --runDB $ updateEpisodeStatus accId episodeKey now episodeStatus
                    --runDB $ updateEpisodeViewCount episodeKey (if episodeStatus then 1 else (-1))
                    selectRep $ do
                        let status = "ok" :: Text
                        provideRep $ return $ object
                            [ "status" .= status ]
                _ -> sendResponseStatus HT.status500 ("Invalid JSON request" :: Text)
        _ -> sendResponseStatus HT.status500 ("Expected JSON request" :: Text)



--    return (TypedContent "application/json" (toContent s))

