{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
module Handler.Calendar where

import Import

import           Yesod.Auth (maybeAuthId)
import           Data.Time.LocalTime (LocalTime(..))
import           Data.Maybe (fromJust)
import           Database.Persist.Sql (PersistField(..), PersistFieldSql(..), Single(..), rawSql)
import           Database.Persist (toPersistValue)
import           Text.Shakespeare.Text (st)
import qualified Data.Map as M
import qualified Data.Time.Calendar as C
import qualified Data.Time.Clock as C
import qualified Data.Time.LocalTime as LT
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Format as TF


selectEpisodesForCalendarSql = [st|
    select episode.id
    from
         subscription join show on (subscription.show = show.id)
         join season on (season.show = show.id)
         join episode on (episode.season = season.id)
         left join (
            select episode
            from episode_status
            where episode_status.user = ?) as episode_status
          on (episode_status.episode = episode.id)
    where
         subscription.user = ?
|]

-- Episode data in calendar.
data CalendarEpisode = CalendarEpisode { calendarEpisodeTitle :: Text
                                       , calendarEpisodeSeasonNumber :: Int
                                       , calendarEpisodeNumber :: Int
                                       , calendarEpisodeSeen :: Bool
                                       , calendarEpisodeTime :: LocalTime }


data Day = Day { dayEpisodes :: [CalendarEpisode] }


data Week = Week { weekDays :: [Maybe Day] }


data Month = Month { monthWeeks :: [Week] }


-- Create calendar for template.
-- Calendar is a list of weeks.
-- Each week contains episodes with statuses.
calendar :: Int -> Int -> [CalendarEpisode] -> Month
calendar year month episodes = Month { monthWeeks = [] }


getCalendarR :: Handler Html
getCalendarR = do
    let year = 2014
    let month = 7
    getCalendarMonthR year month


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

    let timeZone = fromJust $ M.lookup timeZoneName timeZoneMap

    -- convert start and end of month to UTC times
    let d1 = C.fromGregorian (toInteger year) month 1
    let d2 = C.addGregorianMonthsClip 1 d1

    let lt1 = LT.LocalTime { localDay = d1, localTimeOfDay = LT.midnight }
    let lt2 = LT.LocalTime { localDay = d2, localTimeOfDay = LT.midnight }

    let utc1 = LT.localTimeToUTC timeZone lt1
    let utc2 = LT.localTimeToUTC timeZone lt2

    -- select
    -- from
    --      subscription join show on (subscription.show_id = show.show_id)
    --      join season on (season.show_id = show.show_id)
    --      join episode on (episode.season_id = season.season_id)
    --      join episode_status on (episode_status.episode_id = episode.episode_id)
    -- where
    --      subscription.user_id = :userId
    --      episode_status.user_id = :userId
    --      episode.air_date_time between :min_date_time and :max_date_time

    (x :: [(Single Int)]) <- case ma of
        Just authId -> do
            let sql = selectEpisodesForCalendarSql
            let params = [toPersistValue authId, toPersistValue authId]
            runDB $ rawSql sql params
        Nothing -> return []

    $(logDebug) $ TL.toStrict $ TF.format "episode ids: {}" $ [show [map (\v -> unSingle v) x]]

    defaultLayout $ do
        setTitle "Episodes"
        $(widgetFile "calendar")

