{-# LANGUAGE OverloadedStrings #-}

module Handler.Export (
    getICalR,
    getICalPageR
) where


import Prelude hiding (Show, show)
import Yesod
import Text.ICalendar
import Data.Default (def)
import Data.Text (Text)
import Data.Time (addUTCTime)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import qualified Data.Text.Format as TF
import qualified Data.Text.Lazy as TL
import qualified Yesod.Auth as YA

import Foundation
import Model
import Episodes.DB (getEpisodesForICal, getProfile)
import Settings (widgetFile)


getICalPageR :: Handler Html
getICalPageR = do
    accId <- YA.requireAuthId
    mp <- runDB $ getProfile accId
    let mCookie = case mp of
            Just _profile -> profileCookie _profile
            Nothing -> Nothing
    defaultLayout $ do
        setTitle "iCal"
        $(widgetFile "ical")


createEpisodeEventUid :: Entity Episode -> TL.Text
createEpisodeEventUid (Entity epId _) = TF.format "ep-{}@e.pisod.es" [toPathPiece epId]


-- Simplify event creation :<
createEvent :: (Entity Show, Entity Season, Entity Episode) -> (TL.Text, VEvent)
createEvent (_eshow, _eseason, _eepisode) = (_uidValue, _event)
    where
        (Entity _ _show) = _eshow
        (Entity _ _season) = _eseason
        (Entity _ _episode) = _eepisode
        _t = episodeAirDateTime _episode
        _tend = addUTCTime (30 * 60) _t
        _uidValue = createEpisodeEventUid _eepisode
        _summary = TF.format "{}: {}" (showTitle _show, episodeTitle _episode)
        _uid = UID _uidValue def
        _vsummary = Summary { summaryValue = _summary
                           , summaryAltRep = Nothing
                           , summaryLanguage = Nothing
                           , summaryOther = def }
        _event = VEvent { veUID = _uid
                        , veDTStamp = DTStamp _t def
                        , veClass = def
                        , veDTStart = Just $ DTStartDateTime (UTCDateTime _t) def
                        , veCreated = Nothing
                        , veDescription = Nothing
                        , veGeo = Nothing
                        , veLastMod = Nothing
                        , veLocation = Nothing
                        , veOrganizer = Nothing
                        , vePriority = def
                        , veSeq = def
                        , veStatus = Nothing
                        , veSummary = Just _vsummary
                        , veTransp = def
                        , veUrl = Nothing
                        , veRecurId = Nothing
                        , veRRule = S.empty
                        , veDTEndDuration = Just $ Left $ DTEndDateTime (UTCDateTime _tend) def
                        , veAttach = S.empty
                        , veAttendee = S.empty
                        , veCategories = S.empty
                        , veComment = S.empty
                        , veContact = S.empty
                        , veExDate = S.empty
                        , veRStatus = S.empty
                        , veRelated = S.empty
                        , veResources = S.empty
                        , veRDate = S.empty
                        , veAlarms = S.empty
                        , veOther = S.empty }


generateICal :: [(Entity Show, Entity Season, Entity Episode)] -> BL.ByteString
generateICal episodes = printICalendar encodingFunctions vCalendar
    where
        encodingFunctions = def
        vCalendar = def { vcProdId = _prodId
                        , vcEvents = _eventMap }
        _prodId = ProdId "episodes" def
        _eventMap = M.fromList _eventList
        _eventList = map epToEventItem episodes
        epToEventItem _sse = (epEventKey, epEvent)
            where
                epEventKey = (epEventUid, Nothing)
                (epEventUid, epEvent) = createEvent _sse



getICalR :: Text -> Handler TypedContent
getICalR _cookie = do
    sse <- runDB $ getEpisodesForICal _cookie
    let icalbs = generateICal sse
    let contentType = "text/calendar" :: BS.ByteString
    return $ toTypedContent (contentType, toContent icalbs)

