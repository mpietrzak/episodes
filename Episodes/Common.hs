
-- |Common stuff used in many places, utility functions only, no business logic.
module Episodes.Common (
    getUserTimeZone
) where


import Prelude
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
