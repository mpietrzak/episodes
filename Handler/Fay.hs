{-# LANGUAGE OverloadedStrings #-}

module Handler.Fay (
    onCommand
)
where


import Fay.Convert (readFromFay)
import Prelude
import Yesod (HandlerT, invalidArgs, logDebug, runDB)
import Yesod.Auth (requireAuthId)
import Yesod.Fay
import Database.Persist
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Format as TF

import Foundation (App)
import Model
-- (EpisodeStatus(..), Unique(..))
import SharedTypes (Command(..), SetEpisodeStatusResult(..))
import Episodes.DB (updateEpisodeViewCount)


setEpisodeStatus :: Int -> Bool -> HandlerT App IO SetEpisodeStatusResult
setEpisodeStatus episodeId status = do
    $(logDebug) $ TL.toStrict $ TF.format "setting status of episode {} to {}" (episodeId, status)
    userId <- requireAuthId
    let episodeKey = Key $ PersistInt64 (fromIntegral episodeId)
    let newStatusText = case status of
            True -> "seen"
            False -> "unseen"
    let newEpisodeStatus = EpisodeStatus { episodeStatusUser = userId
                                         , episodeStatusEpisode = episodeKey
                                         , episodeStatusStatus = newStatusText }
    maybeEpisodeStatusEntity <- runDB $ getBy $ UniqueEpisodeStatusUserEpisode userId episodeKey
    case maybeEpisodeStatusEntity of
        Nothing -> runDB $ insert_ newEpisodeStatus
        Just (Entity k _) -> runDB $ replace k newEpisodeStatus
    _ <- runDB $ updateEpisodeViewCount episodeKey (case status of
                                                        True -> 1
                                                        False -> -1)
    return SetEpisodeStatusResult { setEpisodeStatusResultErrorCode = 0
                                  , setEpisodeStatusResultErrorDesc = Nothing
                                  , setEpisodeStatusResultEpisodeStatus = newStatusText }


onCommand :: CommandHandler App
onCommand render command =
    case readFromFay command of
        Just (SetEpisodeStatus episodeId status r) -> render r =<< setEpisodeStatus episodeId status
        Nothing                                    -> invalidArgs ["Invalid command"]

