{-# LANGUAGE OverloadedStrings #-}

-- | Common stuff used in many places, utility functions only, no business logic.
module Episodes.Common (
    choose,
    defaultUserEpisodeLinks,
    forceText,
    forceLazyText,
    formatInTimeZone,
    formatTime,
    formatEpisodeCode,
    getUserEpisodeLinks,
    getUserTimeZone,
    hashPassword
) where


import Prelude
import Crypto.PBKDF.ByteString (sha1PBKDF2)
import Data.Text (Text)
import Formatting
import Formatting.Time
import Yesod
import Yesod.Auth (maybeAuthId)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64 as BSB64
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Time
import qualified Data.Time.Zones as TZ

import Foundation
import Model
import Episodes.DB (getProfile)


-- | Get users timezone, return default timezone if not found.
getUserTimeZone :: Handler TZ.TZ
getUserTimeZone = do
    app <- getYesod
    mAccId <- maybeAuthId
    let timeZoneMap = appCommonTimeZoneMap app
    -- workaround for TZ's UTC bug
    let defaultTimeZone = case M.lookup "Europe/London" timeZoneMap of
            Just _tz -> _tz
            Nothing -> error "default TZ not found"
    case mAccId of
        Just accId -> do
            mprofile <- runDB $ getProfile accId
            case mprofile of
                Just profile -> do
                    let mTimeZoneName = profileTimezone profile
                    case mTimeZoneName of
                        Just timeZoneName -> do
                            let _tz = M.findWithDefault defaultTimeZone timeZoneName timeZoneMap
                            return _tz
                        Nothing -> return defaultTimeZone
                Nothing -> return defaultTimeZone
        Nothing -> return defaultTimeZone


defaultUserEpisodeLinks :: Text
defaultUserEpisodeLinks = T.intercalate "\n" [
        "https://www.google.com/search?q={{show.title}}+{{episode.code}}",
        "http://tv.com/search?q={{show.title}}+{{episode.title}}",
        "http://www.tvrage.com/search.php?search={{show.title}}+{{episode.code}}"
    ]


getUserEpisodeLinks :: Maybe AccountId -> Handler Text
getUserEpisodeLinks ma = do
    case ma of
        Nothing -> return defaultUserEpisodeLinks
        Just _a -> do
            mprofile <- runDB $ getProfile _a
            case mprofile of
                Nothing -> return defaultUserEpisodeLinks
                Just _p -> do
                    let mEpisodeLinks = profileEpisodeLinks _p
                    case mEpisodeLinks of
                        Nothing -> return defaultUserEpisodeLinks
                        Just _l -> return _l


-- | Helper for hamlet.
forceText :: Text -> Text
forceText = id


-- | Anothrer helper for hamlet, to force given type.
forceLazyText :: TL.Text -> TL.Text
forceLazyText = id


-- | Helper for hamlet
-- TODO: there's Data.Bool.bool…
choose :: Bool -> a -> a -> a
choose t x y = if t then x else y


formatEpisodeCode :: (Integral a, Integral b) => a -> b -> TL.Text
formatEpisodeCode s e = format ("s" % left 2 '0' % "e" % left 2 '0') si ei
    where
        si = fromIntegral s :: Int
        ei = fromIntegral e :: Int


formatTime :: Data.Time.UTCTime -> TL.Text
formatTime t = format (dateDash % " " % hms) t t


formatInTimeZone :: TZ.TZ -> Data.Time.UTCTime -> TL.Text
formatInTimeZone _tz _t = format (dateDash % " " % hms) _lt _lt
    where
        _lt = TZ.utcToLocalTimeTZ _tz _t


hashPassword :: BS.ByteString -> BS.ByteString -> BS.ByteString
hashPassword salt password = hashBS
    where
        rounds = 400
        hashlen = 24
        -- pbkdf2.py salt is $p5k2$$<raw-salt>
        pbkdf2Salt = BS.intercalate "$" ["", "p5k2", "", salt]
        passHashBytes = sha1PBKDF2 password pbkdf2Salt rounds hashlen
        _trhc c = case c of
            '+' -> '.'
            _ -> c
        hashBS = BS.intercalate "$" [
                "",
                "p5k2",
                "",
                salt,
                BS.map _trhc $ BSB64.encode passHashBytes
            ]

