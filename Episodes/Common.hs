
-- |Common stuff used in many places, utility functions only, no business logic.
module Episodes.Common (
    choose,
    getUserTimeZone,
    forceText
) where


import Prelude
import Data.Text (Text)
import Yesod
import Yesod.Auth (maybeAuthId)
import qualified Data.Map.Strict as M
import qualified Data.Time.Zones as TZ

import Foundation
import Model
import Episodes.DB (getProfile)


-- |Get users timezone, return UTC if not found.
getUserTimeZone :: Handler TZ.TZ
getUserTimeZone = do
    app <- getYesod
    mAccId <- maybeAuthId
    case mAccId of
        Just accId -> do
            mprofile <- runDB $ getProfile accId
            case mprofile of
                Just profile -> do
                    let mTimeZoneName = profileTimezone profile
                    case mTimeZoneName of
                        Just timeZoneName -> do
                            let timeZoneMap = commonTimeZoneMap app
                            let tz = M.findWithDefault TZ.utcTZ timeZoneName timeZoneMap
                            return tz
                        Nothing -> return TZ.utcTZ
                Nothing -> return TZ.utcTZ
        Nothing -> return TZ.utcTZ



-- | Helper for hamlet
forceText :: Text -> Text
forceText = id


-- | Helper for hamlet
choose :: Bool -> a -> a -> a
choose t x y = if t then x else y