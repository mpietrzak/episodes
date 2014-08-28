{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}

module Episodes.DB (
    checkPassword,
    createAccount,
    getEpisodeStatusesByShowAndUser,
    getPopularShowsEpisodesByMonth,
    getPopularShows,
    getPopularEpisodes,
    updateEpisodeStatus,
    updateEpisodeViewCount,
    updateShowSubscriptionCount
) where


import Control.Monad.Trans.Resource (MonadResource (..))
import Crypto.PBKDF.ByteString (sha1PBKDF2)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import Database.Persist
import Database.Persist.Sql
import Prelude hiding (Show)
import Text.Shakespeare.Text (st)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64 as BSB64
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


updateEpisodeViewCount :: (PersistQuery m, PersistMonadBackend m ~ SqlBackend) => EpisodeId -> Int -> m ()
updateEpisodeViewCount episodeId change = update episodeId [EpisodeViewCount +=. change]


updateShowSubscriptionCount :: (PersistQuery m, PersistMonadBackend m ~ SqlBackend) => ShowId -> Int -> m ()
updateShowSubscriptionCount showId change = update showId [ShowSubscriptionCount +=. change]


getEpisodeStatusesByShowAndUser :: (MonadSqlPersist m, MonadResource m) => ShowId -> AccountId -> m [Entity EpisodeStatus]
getEpisodeStatusesByShowAndUser showId userId = rawSql selectEpisodeStatusesByShowAndUser [toPersistValue showId, toPersistValue userId]


-- Update episode status.
-- If does not exist, then create new.
updateEpisodeStatus :: (PersistQuery m, PersistUnique m, PersistMonadBackend m ~ SqlBackend)
                    => AccountId
                    -> EpisodeId
                    -> UTCTime
                    -> Bool
                    -> m ()
updateEpisodeStatus accountId episodeKey now status = do
    let newStatusText = case status of
            True -> "seen"
            False -> "unseen"
    maybeEpisodeStatusEntity <- getBy $ UniqueEpisodeStatusAccountEpisode accountId episodeKey
    case maybeEpisodeStatusEntity of
        Nothing -> do
            let newEpisodeStatus = EpisodeStatus { episodeStatusAccount = accountId
                                                 , episodeStatusEpisode = episodeKey
                                                 , episodeStatusStatus = newStatusText
                                                 , episodeStatusCreated = now
                                                 , episodeStatusModified = now }
            insert_ newEpisodeStatus
        Just (Entity k _) -> do
            let u = [EpisodeStatusStatus =. newStatusText, EpisodeStatusModified =. now]
            update k u
    return ()


createAccount :: (PersistStore m, PersistMonadBackend m ~ SqlBackend)
              => Text -> Maybe Text -> UTCTime -> m (Key Account)
createAccount usernameOrEmail mPassword now = do
    let (_username, _email) = if T.any ((==) '@') usernameOrEmail
            then (Nothing, Just usernameOrEmail)
            else (Just usernameOrEmail, Nothing)
    let acc = Account { accountNickname = _username
                      , accountEmail = _email
                      , accountPassword = mPassword
                      , accountAdmin = False
                      , accountAccessedApprox = now
                      , accountCreated = now
                      , accountModified = now }
    accId <- insert acc
    return accId


-- Check if given user/pass match with what is stored in DB.
-- All legacy hashes (from previous DB) are pbkdf2.py "$p5k2$$" hashes (default iterations, random salt), so we don't support anything else.
-- Haskell's pbkdf implementation is more generic than Python, so we have to build salt manually (Python's salt includes prefix etc).
-- Returns True if given pass' hash matches the one from db.
checkPassword :: (PersistUnique m, MonadResource m, PersistEntityBackend Account ~ PersistMonadBackend m)
              => Text
              -> Text
              -> m Bool
checkPassword username password = do
    let rounds = 400
    let hashlen = 24

    maybeAcc <- case T.any ((==) '@') username of
        True -> getBy $ UniqueAccountEmail $ Just username
        False -> getBy $ UniqueAccountNickname $ Just username
    case maybeAcc of
        Nothing -> return False
        Just accEntity -> do
            let acc = entityVal accEntity
            let mDbPassHash = accountPassword acc
            case mDbPassHash of
                Just dbPassHash -> do
                    -- additional helper let for easier logging
                    let dbPassHashBS0 = encodeUtf8 dbPassHash
                    let dbPassHashBS = DT.trace ("password hash from db: " ++ show dbPassHashBS0) dbPassHashBS0
                    let dbPassParts = BS.split '$' dbPassHashBS
                    case dbPassParts of
                        ["", "p5k2", "", "", _] -> do
                            -- pbkdf2.py uses "$p5k2$$xxx" as salt, where xxx is mostly random
                            let pbkdf2Salt0 = BS.intercalate "$" (take 4 dbPassParts)
                            let pbkdf2Salt = DT.trace ("pbkdf2 salt: " ++ show pbkdf2Salt0) pbkdf2Salt0
                            let allegedPassBS = encodeUtf8 password
                            let allegedPassHash = sha1PBKDF2 allegedPassBS pbkdf2Salt rounds hashlen
                            let allegedPassHashB64 = BSB64.encode allegedPassHash
                            let allegedPassHashB64WithSalt0 = BS.intercalate "$" (concat [(take 4 dbPassParts), [allegedPassHashB64]])
                            let allegedPassHashB64WithSalt = DT.trace ("user supplied pass after hashing with salt from db: " ++ show allegedPassHashB64WithSalt0) allegedPassHashB64WithSalt0
                            return (dbPassHashBS == allegedPassHashB64WithSalt)
                        _ -> return False
                _ -> return False



