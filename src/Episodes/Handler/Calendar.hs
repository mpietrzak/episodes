{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}


module Episodes.Handler.Calendar where


import           Control.Applicative
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Data.Bool (bool)
import           Data.Function (on)
import           Data.List (groupBy)
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import           Database.Persist (Entity(Entity), entityKey, entityVal)
import           Formatting ((%), int, sformat, shown)
import           Formatting.Time (datetime)
import           Prelude hiding (Show)
import           Yesod ( Html,
                         addScript,
                         defaultLayout,
                         logDebug,
                         lookupGetParam,
                         notFound,
                         runDB,
                         setTitle,
                         toPathPiece )
import           Yesod.Auth (maybeAuthId)
-- import           Yesod.PureScript (yesodPureScript)
import qualified Data.Map as M
import qualified Data.Text.Read as TR
import qualified Data.Time.Calendar as C
import qualified Data.Time.Calendar.WeekDate as C
import qualified Data.Time.Clock as C
import qualified Data.Time.LocalTime as TLT
import qualified Data.Time.Zones as TZ

import Foundation
import Episodes.Common (forceLazyText, formatEpisodeCode, getUserEpisodeLinks, getUserTimeZone)
import Episodes.DB (getPopularShowsEpisodesByMonth, getUserShowsEpisodesByMonth)
import Episodes.Format (formatMonth)
import Episodes.StaticFiles (js_episodes_js)
import Model
import Settings (widgetFile)



-- Episode data in calendar.
data CalendarEpisode = CalendarEpisode { calendarEpisodeTitle :: Text
                                       , calendarEpisodeShowTitle :: Text
                                       , calendarEpisodeSeasonNumber :: Int
                                       , calendarEpisodeNumber :: Int
                                       , calendarEpisodeSeen :: Bool
                                       , calendarEpisodeTime :: TLT.LocalTime
                                       , calendarEpisodeId :: EpisodeId
                                       , calendarEpisodeShowId :: ShowId
                                       , calendarEpisodeTVRageId :: Maybe Integer }


data Day = Day { dayEpisodes :: [CalendarEpisode]
               , dayDay :: C.Day }


data Week = Week { weekDays :: [Day] }


-- | Full weeks of days for by month, so some days are in fact not from this month.
-- For example if this month ends on monday, then all other days will be from next month.
data Month = Month { monthWeeks :: [Week] }


groupEpisodesByDay :: [CalendarEpisode] -> M.Map C.Day [CalendarEpisode]
groupEpisodesByDay= foldr _add M.empty
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


-- Convert data returned from DB to helper CalendarEpisode.
showSeasonEpisodeToCalendarEpisode :: TZ.TZ -> (Entity Show, Entity Season, Entity Episode, Maybe (Entity EpisodeStatus)) -> Maybe CalendarEpisode
showSeasonEpisodeToCalendarEpisode tz (showEntity, seasonEntity, episodeEntity, maybeEpisodeStatus) =
    case episodeAirDateTime _episode of
        Just _episodeAirDateTime -> Just CalendarEpisode { calendarEpisodeTitle = episodeTitle _episode
                                                         , calendarEpisodeShowTitle = showTitle _show
                                                         , calendarEpisodeSeasonNumber = seasonNumber _season
                                                         , calendarEpisodeNumber = episodeNumber _episode
                                                         , calendarEpisodeSeen = _isSeen maybeEpisodeStatus
                                                         , calendarEpisodeTime = TZ.utcToLocalTimeTZ tz _episodeAirDateTime
                                                         , calendarEpisodeId = entityKey episodeEntity
                                                         , calendarEpisodeShowId = entityKey showEntity
                                                         , calendarEpisodeTVRageId = fmap fromIntegral (showTvRageId _show) }
                where
                    _show = entityVal showEntity
                    _season = entityVal seasonEntity
                    _isSeen _mestatus = case _mestatus of
                        Just (Entity _ _status) -> episodeStatusStatus _status == "seen"
                        _ -> False
        Nothing -> Nothing
    where
           _episode = entityVal episodeEntity


getHomeR :: Handler Html
getHomeR = getCalendarR


-- Get calendar, optionally accept year and month parameters.
getCalendarR :: Handler Html
getCalendarR = do
        -- req <- getRequest
        myear <- mdecimal <$> lookupGetParam "year"
        mmonth <- mdecimal <$> lookupGetParam "month"
        (year, month) <- case (myear, mmonth) of
                (Just y, Just m) -> return (y, m)
                _ -> do
                    now <- lift C.getCurrentTime
                    timeZone <- getUserTimeZone
                    let localTime = TZ.utcToLocalTimeTZ timeZone now
                    let localDay = TLT.localDay localTime
                    let (year, month, _) = C.toGregorian localDay
                    return (year, month)
        getCalendarMonthR (fromInteger year) month
    where
        mdecimal mx = case mx of
            Nothing -> Nothing
            Just x -> case TR.decimal x of
                Left _ -> Nothing
                Right (d, _) -> Just d


getCalendarMonthR :: Int -> Int -> Handler Html
getCalendarMonthR year month =
    if year >= 1900 && year <= 2100 then do
        ma <- maybeAuthId
        now <- liftIO C.getCurrentTime

        timeZone <- getUserTimeZone

        $(logDebug) $ sformat ("user tz: " % shown) timeZone

        let localTime = TZ.utcToLocalTimeTZ timeZone now
        let localDay = TLT.localDay localTime
        let isToday = (==) localDay

        -- convert start and end of month to UTC times
        let d1 = C.fromGregorian (toInteger year) month 1 -- eg 2012 05 01
        let d2 = C.addGregorianMonthsClip 1 d1            -- eg 2012 06 01

        $(logDebug) $ sformat ("d1: " % shown % ", d2: " % shown) d1 d2

        let lt1 = TLT.LocalTime { TLT.localDay = d1, TLT.localTimeOfDay = TLT.midnight }
        let lt2 = TLT.LocalTime { TLT.localDay = d2, TLT.localTimeOfDay = TLT.midnight }

        $(logDebug) $ sformat ("lt1: " % shown % ", lt2: " % shown) lt1 lt2

        let utc1 = TZ.localTimeToUTCTZ timeZone lt1
        let utc2 = TZ.localTimeToUTCTZ timeZone lt2

        $(logDebug) $ sformat ("utc1: " % shown % ", utc2: " % shown) utc1 utc2

        calendarEpisodes <- case ma of
                Just _acc -> do
                    sses <- runDB $ getUserShowsEpisodesByMonth _acc utc1 utc2
                    $(logDebug) $
                        sformat
                            ("got " % int % " user's episodes for " % datetime % " to " % datetime)
                            (length sses)
                            utc1
                            utc2
                    return $ mapMaybe (showSeasonEpisodeToCalendarEpisode timeZone) sses
                Nothing -> do
                    sse <- runDB $ getPopularShowsEpisodesByMonth 32 utc1 utc2
                    let sses = map (\(_show, _season, _episode) -> (_show, _season, _episode, Nothing)) sse
                    $(logDebug) $
                        sformat
                            ("got " % int % " popular episodes for " % datetime % " to " % datetime)
                            (length sses)
                            utc1
                            utc2
                    return $ mapMaybe (showSeasonEpisodeToCalendarEpisode timeZone) sses

        let calendar = createCalendar (toInteger year) month calendarEpisodes

        let prevMonth = C.addGregorianMonthsClip (-1) (C.fromGregorian (toInteger year) month 1)
        let nextMonth = C.addGregorianMonthsClip 1 (C.fromGregorian (toInteger year) month 1)

        episodeLinks <- getUserEpisodeLinks ma

        defaultLayout $ do
            setTitle "Episodes"
            -- addScript $ PureScriptR $ getPureScriptRoute ["Calendar"]
            -- $(addPureScriptWidget yesodPureScriptOptions "Calendar")
            -- $(yesodPureScript development 'PureScriptR yesodPureScriptOptions "Calendar")
            -- addStylesheet $ StaticR js_Calendar_js
            addScript $ StaticR js_episodes_js
            $(widgetFile "calendar")
    else
        notFound

