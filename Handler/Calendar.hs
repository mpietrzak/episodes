{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}


module Handler.Calendar where


import           Control.Monad.Trans.Class (lift)
import           Data.Function (on)
import           Data.List (groupBy)
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import           Database.Persist (Entity(Entity), entityKey, entityVal, fromPersistValue, getBy, toPersistValue)
import           Database.Persist.Sql (Single(..), rawSql)
import           Prelude hiding (Show)
import           Text.Shakespeare.Text (st)
import           Yesod (Html, addScript, defaultLayout, getYesod, runDB, setTitle, toPathPiece)
import           Yesod.Auth (maybeAuthId)
import qualified Data.Map as M
import qualified Data.Time.Calendar as C
import qualified Data.Time.Calendar.WeekDate as C
import qualified Data.Time.Clock as C
import qualified Data.Time.LocalTime as TLT
import qualified Data.Time.Zones as TZ

import Foundation
import Episodes.Common (getUserTimeZone)
import Episodes.DB (getPopularShowsEpisodesByMonth)
import Episodes.Format (formatMonth)
import Episodes.YesodPureScript (getPureScriptRoute)
import Model
import Settings (widgetFile)


selectEpisodesForCalendarSql :: Text
selectEpisodesForCalendarSql = [st|
    select
        episode.title,
        show.title,
        season.number,
        episode.number,
        case when episode_status.id is not null and episode_status.status = 'seen'
            then 1
            else 0
        end as episode_seen,
        episode.air_date_time,
        episode.id,
        show.id
    from
         subscription join show on (subscription.show = show.id)
         join season on (season.show = show.id)
         join episode on (episode.season = season.id)
         left join (
            select episode, id, status
            from episode_status
            where episode_status.account = ?) as episode_status
          on (episode_status.episode = episode.id)
    where
         subscription.account = ?
         and episode.air_date_time >= ?
         and episode.air_date_time <= ?;
|]


-- Episode data in calendar.
data CalendarEpisode = CalendarEpisode { calendarEpisodeTitle :: Text
                                       , calendarEpisodeShowTitle :: Text
                                       , calendarEpisodeSeasonNumber :: Int
                                       , calendarEpisodeNumber :: Int
                                       , calendarEpisodeSeen :: Bool
                                       , calendarEpisodeTime :: TLT.LocalTime
                                       , calendarEpisodeId :: EpisodeId
                                       , calendarEpisodeShowId :: ShowId }


data Day = Day { dayEpisodes :: [CalendarEpisode]
               , dayDay :: C.Day }


data Week = Week { weekDays :: [Day] }


data Month = Month { monthWeeks :: [Week] }


groupEpisodesByDay :: [CalendarEpisode] -> M.Map C.Day [CalendarEpisode]
groupEpisodesByDay episodes = foldr _add M.empty episodes
    where
        _add episode m = M.insert day newDayEpisodes m
            where
                day = TLT.localDay $ calendarEpisodeTime episode
                newDayEpisodes = episode:oldDayEpisodes
                oldDayEpisodes = M.findWithDefault [] day m


getFullWeekDaysOfMonth :: Integer -> Int -> [C.Day]
getFullWeekDaysOfMonth year month = [firstWeekDayOfMonth..lastWeekDayOfMonth]
    where
        monthDayCount = C.gregorianMonthLength year month
        firstDayOfMonth = C.fromGregorian year month 1
        lastDayOfMonth = C.addDays (toInteger monthDayCount) firstDayOfMonth
        -- following two are pairs, because first day of first week can be in prev year
        firstWeekOfMonth = (y, m)
            where
                (y, m, _) = C.toWeekDate firstDayOfMonth
        lastWeekOfMonth = (y, m)
            where
                (y, m, _) = C.toWeekDate lastDayOfMonth
        firstWeekDayOfMonth = C.fromWeekDate (fst firstWeekOfMonth) (snd firstWeekOfMonth) 1 -- monday of first week of month, so possibly before first day of month
        lastWeekDayOfMonth = C.fromWeekDate (fst lastWeekOfMonth) (snd lastWeekOfMonth) 7   -- sunday of last week of month, so possibly after last day of month


-- Create calendar for template.
-- Calendar is a list of weeks.
-- Each week contains episodes with statuses.
createCalendar :: Integer -> Int -> [CalendarEpisode] -> Month
createCalendar year month episodes = calMonth
    where
        episodesByDayMap = groupEpisodesByDay episodes
        getEpisodesByDay d = M.findWithDefault [] d episodesByDayMap

        -- if thuesday is second day of month, then get also monday, so we have complete row of first week
        -- so instead of days of month, we get full-week days of weeks of month
        fullWeekDaysOfMonth = getFullWeekDaysOfMonth year month

        dayWeekCmp = (==) `on` getWeekOfDay
        getWeekOfDay d = (y, w)
            where
                (y, w, _) = C.toWeekDate d
        weeksOfMonth = groupBy dayWeekCmp fullWeekDaysOfMonth

        -- weeksOfMonth is a list of weeks, where each week is a list of 7 days
        -- convert weeksOfMonth to a [Week]
        epCalWeeks = map weekDaysToEpCalDays weeksOfMonth

        -- convert a list of days to week (which contains list of Day instances)
        weekDaysToEpCalDays days = Week { weekDays = map dayToEpCalDay days }

        -- convert day to episode calendar day
        dayToEpCalDay day = Day { dayDay = day
                                , dayEpisodes = getEpisodesByDay day }

        -- epCalWeeks is [Week], so we simply wrap it in month
        calMonth = Month { monthWeeks = epCalWeeks }


-- Convert (Show, Season, Episode) to CalendarEpisode.
showSeasonEpisodeToCalendarEpisode :: TZ.TZ -> (Entity Show, Entity Season, Entity Episode) -> CalendarEpisode
showSeasonEpisodeToCalendarEpisode tz (showEntity, seasonEntity, episodeEntity) = CalendarEpisode { calendarEpisodeTitle = episodeTitle _episode
                                                                                                  , calendarEpisodeShowTitle = showTitle _show
                                                                                                  , calendarEpisodeSeasonNumber = seasonNumber _season
                                                                                                  , calendarEpisodeNumber = episodeNumber _episode
                                                                                                  , calendarEpisodeSeen = False  -- for unauthenticated user every _episode is not seen
                                                                                                  , calendarEpisodeTime = TZ.utcToLocalTimeTZ tz (episodeAirDateTime _episode)
                                                                                                  , calendarEpisodeId = entityKey episodeEntity
                                                                                                  , calendarEpisodeShowId = entityKey showEntity }
    where
        _episode = entityVal episodeEntity
        _show = entityVal showEntity
        _season = entityVal seasonEntity


getHomeR :: Handler Html
getHomeR = getCalendarR


-- Get calendar for default date.
-- Currently this formats calendar for current time, but it will eventually accept optional year and month.
getCalendarR :: Handler Html
getCalendarR = do
    now <- lift C.getCurrentTime
    timeZone <- getUserTimeZone
    let localTime = TZ.utcToLocalTimeTZ timeZone now
    let localDay = TLT.localDay localTime
    let (year, month, _) = C.toGregorian localDay
    getCalendarMonthR (fromInteger year) month


getCalendarMonthR :: Int -> Int -> Handler Html
getCalendarMonthR year month = do
    ma <- maybeAuthId

    timeZone <- getUserTimeZone

    -- convert start and end of month to UTC times
    let d1 = C.fromGregorian (toInteger year) month 1 -- eg 2012 05 01
    let d2 = C.addGregorianMonthsClip 1 d1            -- eg 2012 05 01

    let lt1 = TLT.LocalTime { TLT.localDay = d1, TLT.localTimeOfDay = TLT.midnight }
    let lt2 = TLT.LocalTime { TLT.localDay = d2, TLT.localTimeOfDay = TLT.midnight }

    let utc1 = TZ.localTimeToUTCTZ timeZone lt1
    let utc2 = TZ.localTimeToUTCTZ timeZone lt2

    calendarEpisodeRows <- case ma of
        Just authId -> do
            let sql = selectEpisodesForCalendarSql
            let params = [toPersistValue authId, toPersistValue authId, toPersistValue utc1, toPersistValue utc2]
            runDB $ rawSql sql params
        Nothing -> return []

    popularCalendarEpisodes <- case ma of
        Just _ -> return []
        Nothing -> do
            sse <- runDB $ getPopularShowsEpisodesByMonth 32 utc1 utc2
            return $ map (showSeasonEpisodeToCalendarEpisode timeZone) sse

    let calendarEpisodes = case ma of
            Just _ -> mapMaybe (calEpRowToMaybeCalEp timeZone) calendarEpisodeRows
            Nothing -> popularCalendarEpisodes

    let calendar = createCalendar (toInteger year) month calendarEpisodes

    let prevMonth = C.addGregorianMonthsClip (-1) (C.fromGregorian (toInteger year) month 1)
    let nextMonth = C.addGregorianMonthsClip 1 (C.fromGregorian (toInteger year) month 1)

    defaultLayout $ do
        setTitle "Episodes"
        addScript $ PureScriptR $ getPureScriptRoute ["Calendar"]
        $(widgetFile "calendar")


    where
        -- TODO: gather custom SQL results using Persist's machinery
        calEpRowToMaybeCalEp tz row =
            case (_mTitle,
                    _mShowTitle,
                    _mSeasonNumber,
                    _mEpisodeNumber,
                    _mEpisodeSeen,
                    _mEpisodeTime,
                    _mEpisodeId,
                    _mShowId) of
                (Just _title,
                    Just _showTitle,
                    Just _seasonNumber,
                    Just _episodeNumber,
                    Just _episodeSeen,
                    Just _episodeTime,
                    Just _episodeId,
                    Just _showId) -> Just CalendarEpisode { calendarEpisodeTitle = _title
                                                          , calendarEpisodeShowTitle = _showTitle
                                                          , calendarEpisodeSeasonNumber = _seasonNumber
                                                          , calendarEpisodeNumber = _episodeNumber
                                                          , calendarEpisodeSeen = case _episodeSeen of
                                                                                     1 -> True
                                                                                     _ -> False
                                                          , calendarEpisodeTime = TZ.utcToLocalTimeTZ tz _episodeTime
                                                          , calendarEpisodeId = _episodeId
                                                          , calendarEpisodeShowId = _showId }
                _ -> Nothing
            where
                (_pTitle, _pShowTitle, _pSeasonNumber, _pEpisodeNumber, _pEpisodeSeen, _pEpisodeTime, _pEpisodeId, _pShowId) = row

                (_mTitle :: Maybe Text) = fromSinglePersistValueToMaybe _pTitle
                (_mShowTitle :: Maybe Text) = fromSinglePersistValueToMaybe _pShowTitle
                _mSeasonNumber = fromSinglePersistValueToMaybe _pSeasonNumber
                _mEpisodeNumber = fromSinglePersistValueToMaybe _pEpisodeNumber
                (_mEpisodeSeen :: Maybe Int) = fromSinglePersistValueToMaybe _pEpisodeSeen
                _mEpisodeTime = fromSinglePersistValueToMaybe _pEpisodeTime
                (_mEpisodeId :: Maybe EpisodeId) = fromSinglePersistValueToMaybe _pEpisodeId
                (_mShowId :: Maybe ShowId) = fromSinglePersistValueToMaybe _pShowId

                fromSinglePersistValueToMaybe = eitherToMaybe . fromPersistValue . unSingle
                eitherToMaybe e = case e of
                                    Left _ -> Nothing
                                    Right r -> r


