{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}

module Episodes.DB (
    checkPassword,
    createAccount,
    createEpisode,
    createSeason,
    deleteEmptySeasons,
    deleteEpisodeByEpisodeCode,
    getEpisodesForICal,
    getEpisodeStatusesByShowAndUser,
    getPopularShowsEpisodesByMonth,
    getPopularShows,
    getPopularEpisodes,
    getProfile,
    getShowData,
    getShowsToUpdate,
    getUserShowsEpisodesByMonth,
    setSubscriptionStatus,
    updateEpisodeStatus,
    updateEpisodeViewCount,
    updateShowLastUpdate,
    updateShowNextUpdate,
    updateShowSubscriptionCount
) where


import Control.Monad.IO.Class (MonadIO, liftIO)
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
import qualified Data.Text.IO as TIO
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


-- | Select list of entities of show, season, episode, maybe episode status,
-- based on account id and a range of timestamps.
selectUserShowsEpisodesByMonthSql :: Text
selectUserShowsEpisodesByMonthSql = [st|
    select ??, ??, ??, ??
    from
        subscription join show on (subscription.show = show.id)
        join season on (season.show = show.id)
        join episode on (episode.season = season.id)
        left join (
            select *
            from episode_status
            where episode_status.account = ?) as episode_status
        on (episode_status.episode = episode.id)
    where
        subscription.account = ?
        and episode.air_date_time >= ?
        and episode.air_date_time <= ?
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


selectEpisodesForICal :: Text
selectEpisodesForICal = [st|
    select ??, ??, ??
    from
        profile
        join account on (account.id = profile.account)
        join subscription on (subscription.account = account.id)
        join show on (show.id = subscription.show)
        join season on (season.show = show.id)
        join episode on (episode.season = season.id)
        left join episode_status on (episode_status.episode = episode.id and episode_status.account = account.id)
    where
        profile.cookie = ?
        and (episode_status.id is null or episode_status.status != 'seen')
|]


selectShowsToUpdate :: Text
selectShowsToUpdate = [st|
    select ??
    from show
    where
        next_update <= current_timestamp
        and tv_rage_id is not null
    limit ?
|]


selectShowData :: Text
selectShowData = [st|
    select ??, ??, ??
    from
        show
        join season on (season.show = show.id)
        join episode on (episode.season = season.id)
    where
        show.id = ?
|]


deleteEmptySeasonsSql :: Text
deleteEmptySeasonsSql = [st|
    delete from season
    where id in (
      select season.id
      from
          show
          join season on (season.show = show.id)
          left join episode on (episode.season = season.id)
      where
          show.id = ?
      group by show.id, season.id
      having
          count(episode.id) = 0
    )
|]


deleteEpisodeStatusesByEpisodeCodeSql :: Text
deleteEpisodeStatusesByEpisodeCodeSql = [st|
    delete from episode_status
    where
        id in (
            select episode_status.id
            from
                episode_status join episode on (episode_status.episode = episode.id)
                join season on (season.id = episode.season)
                join show on (season.show = show.id)
            where
                show.id = ?
                and season.number = ?
                and episode.number = ?
        )
|]


deleteEpisodeByEpisodeCodeSql :: Text
deleteEpisodeByEpisodeCodeSql = [st|
    delete from episode
    where id in (
        select episode.id
        from
            episode
            join season on (season.id = episode.season)
            join show on (show.id = season.show)
        where
            show.id = ?
            and season.id = ?
            and episode.id = ?
    )
|]


insertEpisodeSql :: Text
insertEpisodeSql = [st|
    insert into episode (
        title,
        number,
        season,
        air_date_time,
        view_count,
        created,
        modified
    ) values (
        ?,
        ?,
        (select id from season where season.show = ? and season.number = ?),
        ?,
        0,
        ?,
        ?
    )
|]


getPopularShows :: MonadIO m
                => Int
                -> SqlPersistT m [Entity Show]
getPopularShows _count = selectList [] [Desc ShowSubscriptionCount, LimitTo _count]


getPopularEpisodes :: MonadIO m
                   => Int
                   -> SqlPersistT m [(Entity Episode, Entity Season, Entity Show)]
getPopularEpisodes _count = rawSql selectPopularEpisodesSql [toPersistValue _count]


getPopularShowsEpisodesByMonth :: (MonadIO m)
                               => Int
                               -> UTCTime
                               -> UTCTime
                               -> SqlPersistT m [(Entity Show, Entity Season, Entity Episode)]
getPopularShowsEpisodesByMonth cnt t1 t2 = rawSql selectPopularShowsEpisodesByMonthSql [toPersistValue t1, toPersistValue t2, toPersistValue cnt]


getUserShowsEpisodesByMonth :: (MonadIO m)
                           => AccountId
                           -> UTCTime
                           -> UTCTime
                           -> SqlPersistT m [(Entity Show, Entity Season, Entity Episode, Maybe (Entity EpisodeStatus))]
getUserShowsEpisodesByMonth acc t1 t2 = rawSql selectUserShowsEpisodesByMonthSql params
    where
        params = [ toPersistValue acc
                 , toPersistValue acc -- two ? in query for account
                 , toPersistValue t1
                 , toPersistValue t2 ]


updateEpisodeViewCount :: MonadIO m => EpisodeId -> Int -> SqlPersistT m ()
updateEpisodeViewCount episodeId change = update episodeId [EpisodeViewCount +=. change]


updateShowSubscriptionCount :: MonadIO m => ShowId -> Int -> SqlPersistT m ()
updateShowSubscriptionCount showId change = update showId [ShowSubscriptionCount +=. change]


setSubscriptionStatus :: MonadIO m
                      => UTCTime
                      -> AccountId
                      -> ShowId
                      -> Bool
                      -> SqlPersistT m ()
setSubscriptionStatus now accId showId status = do
    case status of
        True -> do
            -- insert new
            let subscription = Subscription {
                    subscriptionAccount = accId,
                    subscriptionShow = showId,
                    subscriptionCreated = now,
                    subscriptionModified = now }
            insert_ subscription
        False -> do
            -- delete
            deleteBy $ UniqueSubscriptionAccountShow accId showId


getEpisodeStatusesByShowAndUser :: MonadIO m => ShowId -> AccountId -> SqlPersistT m [Entity EpisodeStatus]
getEpisodeStatusesByShowAndUser showId userId = rawSql selectEpisodeStatusesByShowAndUser [toPersistValue showId, toPersistValue userId]


getEpisodesForICal :: MonadIO m => Text -> SqlPersistT m [(Entity Show, Entity Season, Entity Episode)]
getEpisodesForICal _cookie = rawSql selectEpisodesForICal [toPersistValue _cookie]


-- Update episode status.
-- If does not exist, then create new.
updateEpisodeStatus :: MonadIO m
                    => AccountId
                    -> EpisodeId
                    -> UTCTime
                    -> Bool
                    -> SqlPersistT m ()
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


createAccount :: MonadIO m
              => Text -> Maybe Text -> UTCTime -> SqlPersistT m (Key Account)
createAccount usernameOrEmail mPassword now = do
    let (_username, _email) = if T.any ((==) '@') usernameOrEmail
            then (Nothing, Just usernameOrEmail)
            else (Just usernameOrEmail, Nothing)
    let acc = Account { accountNickname = _username
                      , accountEmail = _email
                      , accountPassword = mPassword
                      , accountAdmin = False
                      , accountAccessedApprox = now
                      , accountViews = 0
                      , accountCreated = now
                      , accountModified = now }
    accId <- insert acc
    return accId


-- Check if given user/pass match with what is stored in DB.
-- All legacy hashes (from previous DB) are pbkdf2.py "$p5k2$$" hashes (default iterations, random salt), so we don't support anything else.
-- Haskell's pbkdf implementation is more generic than Python, so we have to build salt manually (Python's salt includes prefix etc).
-- Returns True if given pass' hash matches the one from db.
checkPassword :: MonadIO m
              => Text
              -> Text
              -> SqlPersistT m Bool
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


getProfile :: MonadIO m
           => AccountId
           -> SqlPersistT m (Maybe Profile)
getProfile accId = do
    mep <- getBy (UniqueProfileAccount accId)
    case mep of
        Just (Entity _ p) -> return (Just p)
        _ -> return Nothing


getShowsToUpdate :: MonadIO m => Int -> SqlPersistT m [Entity Show]
getShowsToUpdate _cnt = rawSql selectShowsToUpdate [toPersistValue _cnt]


getShowData :: MonadIO m
            => ShowId
            -> SqlPersistT m [(Entity Show, Entity Season, Entity Episode)]
getShowData showKey = rawSql selectShowData [toPersistValue showKey]


updateShowLastUpdate :: MonadIO m
                     => ShowId
                     -> UTCTime
                     -> SqlPersistT m ()
updateShowLastUpdate showKey time = update showKey [ShowLastUpdate =. time]


updateShowNextUpdate :: MonadIO m
                     => ShowId
                     -> UTCTime
                     -> SqlPersistT m ()
updateShowNextUpdate showKey time = update showKey [ShowNextUpdate =. time]


-- | Delete episode and data that references given episode.
-- episodeCode is a tuple of Integrals which are converted to Ints in impl - I'd prefer to expose
-- generic interface. If we're outside of range, then we'll fail.
deleteEpisodeByEpisodeCode :: (MonadIO m, Integral a, Integral b)
                           => ShowId
                           -> (a, b)
                           -> SqlPersistT m ()
deleteEpisodeByEpisodeCode showId episodeCode = do
    let sc = fst episodeCode
    let ec = snd episodeCode
    let isc = fromIntegral sc :: Int
    let iec = fromIntegral ec :: Int
    let _pcl = [toPersistValue showId, toPersistValue isc, toPersistValue iec]
    rawExecute deleteEpisodeStatusesByEpisodeCodeSql _pcl
    rawExecute deleteEpisodeByEpisodeCodeSql _pcl


createSeason :: MonadIO m
             => ShowId
             -> UTCTime
             -> Integer
             -> SqlPersistT m ()
createSeason showId now _seasonNumber = do
    liftIO $ TIO.putStrLn $ T.concat $ ["inserting season ", T.pack $ show _seasonNumber, " of show ", T.pack $ show showId]
    insert_ season
    where
        season = Season { seasonNumber = fromIntegral _seasonNumber
                        , seasonShow = showId
                        , seasonCreated = now
                        , seasonModified = now }


deleteEmptySeasons :: MonadIO m => ShowId -> SqlPersistT m ()
deleteEmptySeasons showId = rawExecute deleteEmptySeasonsSql params
    where
        params = [toPersistValue showId]


createEpisode :: (MonadIO m, Integral a, Integral b)
              => ShowId
              -> a
              -> b
              -> Text
              -> UTCTime
              -> UTCTime
              -> SqlPersistT m ()
createEpisode _showId _seasonNumber _episodeNumber _title _airDateTime _now = rawExecute insertEpisodeSql params
    where
        _seasonNumberInt = fromIntegral _seasonNumber :: Int
        _episodeNumberInt = fromIntegral _episodeNumber :: Int
        params = [
                toPersistValue _title,
                toPersistValue _episodeNumberInt,
                toPersistValue _showId,
                toPersistValue _seasonNumberInt,
                toPersistValue _airDateTime,
                toPersistValue _now,
                toPersistValue _now
            ]

