{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
module Handler.Calendar where

import Import

import           Data.Function (on)
import           Data.List (groupBy)
import           Data.Maybe (mapMaybe)
import           Database.Persist.Sql (Single(..), rawSql)
import           Episodes.Trace (traceValue)
import           Language.Haskell.TH
import           Text.Shakespeare.Text (st)
import           Yesod.Auth (maybeAuthId)
import qualified Data.Map as M
import qualified Data.Time.Calendar as C
import qualified Data.Time.Calendar.WeekDate as C
import qualified Data.Time.Clock as C
import qualified Data.Time.LocalTime as TLT
import qualified Data.Time.Zones as TZ

import Episodes.Format (formatMonth)


selectEpisodesForCalendarSql :: Text
selectEpisodesForCalendarSql = [st|
    select
        episode.title,
        show.title,
        season.number,
        episode.number,
        case when episode_status.id is not null then 1 else 0 end as episode_seen,
        episode.air_date_time,
        episode.id,
        show.id
    from
         subscription join show on (subscription.show = show.id)
         join season on (season.show = show.id)
         join episode on (episode.season = season.id)
         left join (
            select episode, id
            from episode_status
            where episode_status.user = ?) as episode_status
          on (episode_status.episode = episode.id)
    where
         subscription.user = ?
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


getHomeR :: Handler Html
getHomeR = getCalendarR


getCalendarR :: Handler Html
getCalendarR = do
    now <- lift C.getCurrentTime
    ma <- maybeAuthId
    app <- getYesod

    let timeZoneMap = commonTimeZoneMap app
    timeZoneName <- case ma of
            Just authId -> do
                maybeProfileEntity <- runDB $ getBy (UniqueProfileUser authId)
                case maybeProfileEntity of
                    Just (Entity _ profile) -> do
                        return (profileTimezone profile)
                    _ -> return "UTC"
            _ -> return "UTC"
    let timeZone = M.findWithDefault TZ.utcTZ timeZoneName timeZoneMap

    let localTime = TZ.utcToLocalTimeTZ timeZone now
    let localDay = TLT.localDay localTime
    let (year, month, _) = C.toGregorian localDay

    getCalendarMonthR (fromInteger year) month


getCalendarMonthR :: Int -> Int -> Handler Html
getCalendarMonthR year month = do

    ma <- maybeAuthId
    app <- getYesod

    let timeZoneMap = commonTimeZoneMap app

    timeZoneName <- case ma of
            Just authId -> do
                maybeProfileEntity <- runDB $ getBy (UniqueProfileUser authId)
                case maybeProfileEntity of
                    Just (Entity _ profile) -> do
                        return (profileTimezone profile)
                    _ -> return "UTC"
            _ -> return "UTC"

    let timeZone = M.findWithDefault TZ.utcTZ timeZoneName timeZoneMap

    -- convert start and end of month to UTC times
    let d1 = C.fromGregorian (toInteger year) month 1
    let d2 = C.addGregorianMonthsClip 1 d1

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

    let calendarEpisodes = mapMaybe (calEpRowToMaybeCalEp timeZone) calendarEpisodeRows

    let calendar = createCalendar (toInteger year) month calendarEpisodes

    let prevMonth = C.addGregorianMonthsClip (-1) (C.fromGregorian (toInteger year) month 1)
    let nextMonth = C.addGregorianMonthsClip 1 (C.fromGregorian (toInteger year) month 1)

    defaultLayout $ do
        setTitle "Episodes"
        $(fayFile' (ConE 'StaticR) "Calendar")
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


