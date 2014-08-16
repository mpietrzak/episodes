{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}

module Episodes.DB (
    checkPassword,
    getEpisodeStatusesByShowAndUser,
    getPopularShowsEpisodesByMonth,
    getPopularShows,
    getPopularEpisodes,
    updateEpisodeViewCount,
    updateShowSubscriptionCount
) where


import Control.Monad.Trans.Resource (MonadResource (..))
import Crypto.PBKDF.ByteString (sha1PBKDF2)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import Database.Persist
import Database.Persist.Sql (MonadSqlPersist (..), RawSql (..), rawSql)
import Prelude hiding (Show)
import Text.Shakespeare.Text (st)
import qualified Data.Text as T
import qualified Debug.Trace as DT

import Model


selectPopularEpisodesSql :: Text
selectPopularEpisodesSql = [st|
    select
        ??,
        ??,
        ??
    from
        episode
        join season on (episode.season = season.id)
        join show on (season.show = show.id)
    order by episode.view_count desc
    limit ?
|]


-- SQL query used with rawSql that returns [(Show, Season, Episode)]
-- Query returns episodes by utc time of month with seasons and shows.
-- Two dates represent month, becuase those dates are computed
-- by converting user's local month to absolute date-times.
selectPopularShowsEpisodesByMonthSql :: Text
selectPopularShowsEpisodesByMonthSql = [st|
    select
        ??, ??, ??
    from
        episode
        join season on (episode.season = season.id)
        join show on (season.show = show.id)
    where
        episode.air_date_time >= ?
        and episode.air_date_time <= ?
        and show.id in (
            select show.id
            from show
            order by subscription_count desc
            limit ?
        )
|]


selectEpisodeStatusesByShowAndUser :: Text
selectEpisodeStatusesByShowAndUser = [st|
    select ??
    from
        show
        join season on (season.show = show.id)
        join episode on (episode.season = season.id)
        join episode_status on (episode_status.episode = episode.id)
    where
        show.id = ?
        and episode_status.account = ?
|]


getPopularShows :: (PersistQuery m, PersistEntityBackend Show ~ PersistMonadBackend m)
                => Int
                -> m [Entity Show]
getPopularShows _count = selectList [] [Desc ShowSubscriptionCount, LimitTo _count]


getPopularEpisodes :: (MonadSqlPersist m, MonadResource m)
                   => Int
                   -> m [(Entity Episode, Entity Season, Entity Show)]
getPopularEpisodes _count = rawSql selectPopularEpisodesSql [toPersistValue _count]


getPopularShowsEpisodesByMonth :: (MonadSqlPersist m, MonadResource m)
                               => Int
                               -> UTCTime
                               -> UTCTime
                               -> m [(Entity Show, Entity Season, Entity Episode)]
getPopularShowsEpisodesByMonth cnt t1 t2 = rawSql selectPopularShowsEpisodesByMonthSql [toPersistValue t1, toPersistValue t2, toPersistValue cnt]


updateEpisodeViewCount episodeId change = update episodeId [EpisodeViewCount +=. change]


updateShowSubscriptionCount showId change = update showId [ShowSubscriptionCount +=. change]


getEpisodeStatusesByShowAndUser showId userId = rawSql selectEpisodeStatusesByShowAndUser [toPersistValue showId, toPersistValue userId]


checkPassword :: (PersistUnique m, MonadResource m, PersistEntityBackend Account ~ PersistMonadBackend m)
              => Text
              -> Text
              -> m Bool
checkPassword username password = do
    let passwordBS = encodeUtf8 password :: ByteString
    let passwordPBKDF = DT.trace ("sha1PBKDF2 " ++ show passwordBS ++ " -> " ++ show (sha1PBKDF2 "123" passwordBS 1000 16)) $ sha1PBKDF2 "123" passwordBS 1000 16 :: ByteString
    maybeAcc <- case T.any ((==) '@') username of
        True -> getBy $ UniqueAccountEmail $ Just username
        False -> getBy $ UniqueAccountNickname $ Just username
    case maybeAcc of
        Nothing -> return False
        Just accEntity -> do
            let acc = entityVal accEntity
            let mDbPassHash = accountPassword acc
            case mDbPassHash of
                Just ph -> do
                    let phbs = encodeUtf8 ph
                    return (phbs == passwordPBKDF)
                _ -> return False



