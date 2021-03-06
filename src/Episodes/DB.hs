{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Episodes.DB (
    addPrivateShow,
    addShowEpisodes,
    addShowSeasons,
    checkPassword,
    createEpisode,
    createSeason,
    deleteEmptySeasons,
    deleteEpisodeByEpisodeCode,
    deleteShowSeason,
    getAccountByEmail,
    getEpisodeByShowAndCode,
    getEpisodesForICal,
    getEpisodeStatusesByShowAndUser,
    getPopularEpisodes,
    getPopularShows,
    getPopularShowsEpisodesByMonth,
    getProfile,
    getRecentlyPopularEpisodes,
    getRecentEpisodeStatuses,
    getSeasonByShowAndSeasonNumber,
    getShowById,
    getShowSeasonCollapse,
    getShowData,
    getShowEpisodes,
    getShowSeasons,
    getShowsToUpdate,
    getUserShowsEpisodesByMonth,
    getUserShowsEpisodesForExport,
    getUserShowsEpisodesLastSeen,
    setSeasonCollapse,
    setSubscriptionStatus,
    updateEpisode,
    updateEpisode2,
    updateEpisodeStatus,
    updateEpisodeViewCount,
    updateSeason,
    updateShowLastUpdate,
    updateShowNextUpdate,
    updateShowSubscriptionCount
) where


-- import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger(..))
import Crypto.PBKDF.ByteString (sha1PBKDF2)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import Database.Persist
import Database.Persist.Sql
import Formatting ((%), sformat, int)
import Prelude hiding (Show)
import Text.Shakespeare.Text (st)
import Yesod (logDebug)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64 as BSB64
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Debug.Trace as DT

import Model

-- import Formatting (sformat, string)

-- import Control.Monad.Trans.Resource (MonadResource(..))

-- import qualified Data.Conduit as C
-- import qualified Data.Conduit.List as CL


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
    where
        show.public
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
        show.public
        and episode.air_date_time >= ?
        and episode.air_date_time <= ?
    order by show.subscription_count desc
    limit ?
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
            and season.number = ?
            and episode.number = ?
    )
|]


deleteShowSeason :: MonadIO m => ShowId -> Int -> SqlPersistT m ()
deleteShowSeason _showId _seasonNumber = deleteBy $ UniqueShowSeasonNumber _showId _seasonNumber


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


-- | This query gets latest (numbering-wise) episode for each show subscribed by user.
-- This probably should be rewritten to use nl subqueries.
selectUserShowsEpisodesForExport :: Text
selectUserShowsEpisodesForExport = [st|
    select ??, ??, ??
    from
        show
        join season on (season.show = show.id)
        join episode on (episode.season = season.id)
    where
        episode.id in (
            select e.id
            from (
                select
                    episode.id,
                    row_number() over (partition by season.show order by season.number desc, episode.number desc) as p
                from
                    episode_status
                    join episode on (episode.id = episode_status.episode)
                    join season on (season.id = episode.season)
                where
                    episode_status.status = 'seen'
                    and episode_status.account = ?
            ) as e
            where e.p = 1
        )
    order by
        show.title,
        show.id
|]


addPrivateShow :: MonadIO m => UTCTime -> AccountId -> Text -> SqlPersistT m (Either Text ShowId)
addPrivateShow now accountId title = Right <$> insert _show
    where
        _show = Show { showTitle = title,
                       showTvRageId = Nothing,
                       showSubscriptionCount = 0,
                       showNextUpdate = Nothing,
                       showLastUpdate = Nothing,
                       showPublic = False,
                       showSubmitted = False,
                       showLocal = True,
                       showAddedBy = Just accountId,
                       showModified = now,
                       showCreated = now }


addShowEpisodes :: MonadIO m => UTCTime -> ShowId -> Int -> Int -> SqlPersistT m ()
addShowEpisodes _now _showId _seasonNumber _episodeCount = do
    mseason <- getBy $ UniqueShowSeasonNumber _showId _seasonNumber
    case mseason of
        Just (Entity seasonId _season) -> do
            update _showId [ShowModified =. _now]
            nextEpisodeNumber <- do
                _rows <- rawSql "select max(number) from episode where season = ?" [toPersistValue seasonId]
                return $ case _rows of
                    [sv] -> case unSingle sv of
                        Just c -> c + 1
                        Nothing -> 1
                    _ -> 1
            forM_ [0.._episodeCount-1] $ \i -> insert_ $ Episode { episodeTitle = sformat
                                                                                    ("Season " % int % " Episode " % int)
                                                                                    _seasonNumber
                                                                                    (nextEpisodeNumber + i),
                                                                 episodeSeason = seasonId,
                                                                 episodeNumber = nextEpisodeNumber + i,
                                                                 episodeAirDateTime = Nothing,
                                                                 episodeViewCount = 0,
                                                                 episodeCreated = _now,
                                                                 episodeModified = _now }
        Nothing -> return ()


addShowSeasons :: MonadIO m => UTCTime -> ShowId -> Int -> SqlPersistT m ()
addShowSeasons now showId _count = do
    update showId [ShowModified =. now]
    nextSeasonNumber <- do
        rows <- rawSql "select max(number) from season where show = ?" [toPersistValue showId]
        return $ case rows of
            [sv] -> case unSingle sv of
                Just _i -> _i + 1
                Nothing -> 1
            _ -> 1
    forM_ [0.._count-1] $ \i -> insert_ Season { seasonNumber = nextSeasonNumber + i,
                                               seasonShow = showId,
                                               seasonCreated = now,
                                               seasonModified = now }



getAccountByEmail :: MonadIO m => Text -> SqlPersistT m (Maybe (Entity Account))
getAccountByEmail _email = rawSql _sql _params >>= \_rows -> return (_maybeFirst _rows)
    where
        _sql = "select ?? from account where lower(email) = lower(?) order by created asc limit 1"
        _params = [toPersistValue _email]
        _maybeFirst [] = Nothing
        _maybeFirst (x:_) = Just x


getPopularShows :: MonadIO m
                => Int
                -> SqlPersistT m [Entity Show]
getPopularShows _count = selectList [ShowPublic ==. True] [Desc ShowSubscriptionCount, LimitTo _count]


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


getRecentlyPopularEpisodes :: (MonadIO m, Functor m)
                           => Int
                           -> Int
                           -> SqlPersistT m [(Entity Show, Entity Season, Entity Episode)]
getRecentlyPopularEpisodes sampleSize cnt = rawSql _sql _params
    where
        _sql = [st|
                select ??, ??, ??
                from
                    (
                        select id, count(1) as c
                        from
                            (
                                select episode_status.episode as id
                                from
                                    episode_status
                                    join episode on (episode.id = episode_status.episode)
                                    join season on (season.id = episode.season)
                                    join show on (show.id = season.show)
                                where
                                    episode_status.status = 'seen'
                                    and show.public
                                order by episode_status.modified desc
                                limit ?
                            ) as recent_episode_status
                        group by id
                    ) as recent_episode
                    join episode on (episode.id = recent_episode.id)
                    join season on (season.id = episode.season)
                    join show on (show.id = season.show)
                order by
                    recent_episode.c desc,
                    episode.id desc
                limit ?
            |]
        _params = [toPersistValue sampleSize, toPersistValue cnt]


getRecentEpisodeStatuses :: MonadIO m
                         => Int
                         -> SqlPersistT m [(Entity Show, Entity Season, Entity Episode, Entity Account, Entity EpisodeStatus)]
getRecentEpisodeStatuses cnt = rawSql _sql _params
    where
        _sql = [st|
                select ??, ??, ??, ??, ??
                from
                    episode_status
                    join account on (account.id = episode_status.account)
                    join episode on (episode.id = episode_status.episode)
                    join season on (season.id = episode.season)
                    join show on (show.id = season.show)
                order by
                    episode_status.modified desc
                limit ?
            |]
        _params = [toPersistValue cnt]


getSeasonByShowAndSeasonNumber :: MonadIO m => ShowId -> Int -> SqlPersistT m (Maybe (Entity Season))
getSeasonByShowAndSeasonNumber _showId _seasonNumber = getBy $ UniqueShowSeasonNumber _showId _seasonNumber


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



getUserShowsEpisodesLastSeenSql :: Text
getUserShowsEpisodesLastSeenSql = [st|
    select
        show.id,
        show.title,
        last_season.number,
        last_episode.number,
        last_episode.title,
        last_episode.air_date_time,
        next_season.number,
        next_episode.number,
        next_episode.title,
        next_episode.air_date_time
    from
        subscription
        join show on (show.id = subscription.show)
        left join episode as last_episode on (last_episode.id = subscription.last_episode)
        left join season as last_season on (last_season.id = last_episode.season)
        left join episode as next_episode on (next_episode.id = subscription.next_episode)
        left join season as next_season on (next_season.id = next_episode.season)
    where
        subscription.account = ?
    order by
        (abs (extract(epoch from (current_timestamp - coalesce(last_episode.air_date_time, next_episode.air_date_time)))))
|]


-- | [(show, last ep, next ep)]
getUserShowsEpisodesLastSeen :: (MonadIO m)
                             => AccountId
                             -> SqlPersistT m [
                                        -- each row is tuple of:
                                        -- show info
                                        -- maybe last episode info
                                        -- maybe next episode info
                                        (
                                            (ShowId, Text),
                                            Maybe (Int, Int, Text, UTCTime),
                                            Maybe (Int, Int, Text, UTCTime)
                                        )
                                    ]
getUserShowsEpisodesLastSeen accId = rawSql getUserShowsEpisodesLastSeenSql params >>= (\rows -> return (map _unsr rows))
    where
        params = [ toPersistValue accId ]
        _uns2 (a, b) = (unSingle a, unSingle b)
        _uns4 (a, b, c, d) = (unSingle a, unSingle b, unSingle c, unSingle d)
        _unsr (s, le, ne) = (_uns2 s, _uns4 <$> le, _uns4 <$> ne)


-- | Query that returns for given user for each subscribed show, the latest season and episode watched
-- (that is, the one with greatest season number and episode number).
getUserShowsEpisodesForExport :: (MonadIO m)
                              => AccountId
                              -> SqlPersistT m [(Entity Show, Entity Season, Entity Episode)]
getUserShowsEpisodesForExport accountId = rawSql selectUserShowsEpisodesForExport params
    where
        params = [ toPersistValue accountId ]


updateEpisodeViewCount :: MonadIO m => EpisodeId -> Int -> SqlPersistT m ()
updateEpisodeViewCount episodeId change = update episodeId [EpisodeViewCount +=. change]


getSubscriptionIdByAccIdEpisodeId :: MonadIO m => AccountId -> EpisodeId -> SqlPersistT m (Maybe SubscriptionId)
getSubscriptionIdByAccIdEpisodeId accId epId = do
        slm <- rawSql _sql _params -- list of Single (Maybe SubscriptionId)
        let lm = map unSingle slm  -- list of Maybe SubscriptionId
        return (_headMay lm)       -- Maybe SubscriptionId
    where
        _sql = [st|
                select subscription.id
                from
                    episode
                    join season on (season.id = episode.season)
                    join show on (show.id = season.show)
                    join subscription on (subscription.show = show.id)
                where
                    subscription.account = ?
                    and episode.id = ?
            |]
        _params = [
                toPersistValue accId,
                toPersistValue epId
            ]
        -- Note: this is not safe's headMay, it works on list of maybes.
        _headMay :: [Maybe x] -> Maybe x
        _headMay l = case l of
                [] -> Nothing
                _ -> head l


updateShowSubscriptionCount :: MonadIO m => ShowId -> Int -> SqlPersistT m ()
updateShowSubscriptionCount showId change = update showId [ShowSubscriptionCount +=. change]


updateShowSubscriptionLastNextEpisodeSql :: Text
updateShowSubscriptionLastNextEpisodeSql = [st|
    update subscription
    set
        last_episode = (
            select
                episode.id
            from
                show
                join season on (season.show = show.id)
                join episode on (episode.season = season.id)
                join episode_status on (episode_status.episode = episode.id and episode_status.status = 'seen')
                join account on (episode_status.account = account.id)
            where
                show.id = subscription.show
                and account.id = subscription.account
            order by
                season.number desc, episode.number desc
            limit 1
        ),
        next_episode = (
            select
                episode.id
            from
                show
                join season on (season.show = show.id)
                join episode on (episode.season = season.id)
left            join (
                        select * from episode_status where account = subscription.account
                    ) as episode_status
                    on (episode_status.episode = episode.id and episode_status.status = 'seen')
            where
                show.id = subscription.show
                and (
                        episode_status.status != 'seen'
                        or episode_status.status is null
                    )
            order by
                season.number asc, episode.number asc
            limit 1
        )
    where
        subscription.id = ?
|]


updateShowSubscriptionLastNextEpisode :: MonadIO m => SubscriptionId -> SqlPersistT m ()
updateShowSubscriptionLastNextEpisode subscriptionId = rawExecute updateShowSubscriptionLastNextEpisodeSql params
    where
        params = [toPersistValue subscriptionId]


setSeasonCollapse :: MonadIO m => Bool -> AccountId -> SeasonId -> SqlPersistT m ()
setSeasonCollapse collapsed accountId seasonId = upsert _val _update >> return ()
    where
        _val = SeasonCollapse { seasonCollapseAccount = accountId
                              , seasonCollapseSeason = seasonId
                              , seasonCollapseCollapsed = collapsed }
        _update = [ SeasonCollapseCollapsed =. collapsed ]


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
                    subscriptionLastEpisode = Nothing,
                    subscriptionNextEpisode = Nothing,
                    subscriptionCreated = now,
                    subscriptionModified = now }
            subscriptionId <- insert subscription
            updateShowSubscriptionLastNextEpisode subscriptionId
        False -> do
            -- delete
            deleteBy $ UniqueSubscriptionAccountShow accId showId


getEpisodeStatusesByShowAndUser :: MonadIO m => ShowId -> AccountId -> SqlPersistT m [Entity EpisodeStatus]
getEpisodeStatusesByShowAndUser showId userId = rawSql selectEpisodeStatusesByShowAndUser [toPersistValue showId, toPersistValue userId]


getEpisodeByShowAndCode :: MonadIO m => ShowId -> (Int, Int) -> SqlPersistT m (Maybe (Entity Episode))
getEpisodeByShowAndCode _showId (_seasonNumber, _episodeNumber) = do
    _mseason <- getBy $ UniqueShowSeasonNumber _showId _seasonNumber
    case _mseason of
        Just (Entity _seasonId _) -> do
            getBy $ UniqueSeasonEpisodeNumber _seasonId _episodeNumber
        Nothing -> return Nothing


getEpisodesForICal :: MonadIO m => Text -> SqlPersistT m [(Entity Show, Entity Season, Entity Episode)]
getEpisodesForICal _cookie = rawSql selectEpisodesForICal [toPersistValue _cookie]


updateEpisode :: MonadIO m
              => EpisodeId
              -> Text
              -> UTCTime
              -> UTCTime
              -> SqlPersistT m ()
updateEpisode _episodeId _episodeTitle _episodeAirDateTime _episodeModified = update _episodeId _updates
    where
        _updates = [EpisodeTitle =. _episodeTitle,
                    EpisodeAirDateTime =. Just _episodeAirDateTime,
                    EpisodeModified =. _episodeModified ]


updateEpisode2 :: MonadIO m
               => UTCTime
               -> (ShowId, Int, Int)
               -> Text
               -> Int
               -> UTCTime
               -> SqlPersistT m ()
updateEpisode2 _now (_showId, _seasonNumber, _episodeNumber) _title _number _airTime = do
    _mshow <- get _showId
    case _mshow of
        Just _show -> do
            _mseason <- getBy $ UniqueShowSeasonNumber _showId _seasonNumber
            case _mseason of
                Just (Entity _seasonId _season) -> do
                    _mepisode <- getBy $ UniqueSeasonEpisodeNumber _seasonId _episodeNumber
                    case _mepisode of
                        Just (Entity _episodeId _) -> do
                            update _episodeId [ EpisodeTitle =. _title
                                              , EpisodeNumber =. _number
                                              , EpisodeAirDateTime =. Just _airTime
                                              , EpisodeModified =. _now ]
                        Nothing -> return ()
                Nothing -> return ()
        Nothing -> return ()


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
    mSubscriptionId <- getSubscriptionIdByAccIdEpisodeId accountId episodeKey
    case mSubscriptionId of
        Just subscriptionId -> updateShowSubscriptionLastNextEpisode subscriptionId
        _ -> return ()
    return ()


-- | Check if given user/pass match with what is stored in DB.
-- All legacy hashes (from previous DB) are pbkdf2.py "$p5k2$$" hashes (default iterations, random salt),
-- so we wont't support anything else here.
-- Haskell's pbkdf implementation is more generic than Python,
-- so we have to build salt manually (Python's salt includes prefix etc, while Haskells "salt" is just bits).
-- Returns True if given pass' hash matches the one from db.
checkPassword :: MonadIO m
              => Text
              -> Text
              -> SqlPersistT m Bool
checkPassword username password = do

    -- these are the defaults from legacy
    let rounds = 400
    let hashlen = 24

    -- we search by email if username contains '@', otherwise we search by username
    maybeAcc <- case T.any ((==) '@') username of
            True -> getAccountByEmail username
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
                        ["", "p5k2", "", _, _] -> do
                            -- pbkdf2.py uses "$p5k2$$xxx" as salt, where xxx is mostly random
                            let pbkdf2Salt0 = BS.intercalate "$" (take 4 dbPassParts)
                            let pbkdf2Salt = DT.trace ("pbkdf2 salt: " ++ show pbkdf2Salt0) pbkdf2Salt0
                            let allegedPassBS = encodeUtf8 password
                            let allegedPassHash = sha1PBKDF2 allegedPassBS pbkdf2Salt rounds hashlen
                            let _trhc c = case c of
                                    '+' -> '.'
                                    _ -> c
                            let allegedPassHashB64 = BS.map _trhc $ BSB64.encode allegedPassHash
                            let allegedPassHashB64WithSalt0 = BS.intercalate "$" (concat [(take 4 dbPassParts), [allegedPassHashB64]])
                            let allegedPassHashB64WithSalt = DT.trace ("user supplied pass after hashing with salt from db: " ++ show allegedPassHashB64WithSalt0) allegedPassHashB64WithSalt0
                            return (dbPassHashBS == allegedPassHashB64WithSalt)
                        _ -> return False
                _ -> return False


getShowSeasonCollapse :: MonadIO m => AccountId -> ShowId -> SqlPersistT m (SeasonId -> Bool)
getShowSeasonCollapse accId showId = do
        _sc <- rawSql selectShowSeasonCollapse params
        let m = M.fromList $ map (\(_seasonId, _isCollapsed) -> (unSingle _seasonId, unSingle _isCollapsed)) _sc
        let l = \k -> M.findWithDefault False k m
        return l
    where
        selectShowSeasonCollapse = [st|
            select season.id, season_collapse.collapsed
            from
                show
                join season on (season.show = show.id)
                join season_collapse on (season_collapse.season = season.id)
            where
                show.id = ?
                and season_collapse.account = ?
        |]
        params = [ toPersistValue showId
                 , toPersistValue accId ]


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


getShowById :: MonadIO m => ShowId -> SqlPersistT m (Maybe Show)
getShowById = get


getShowData :: MonadIO m
            => ShowId
            -> SqlPersistT m [(Entity Show, Entity Season, Entity Episode)]
getShowData showKey = rawSql selectShowData [toPersistValue showKey]


getShowEpisodes :: MonadIO m
                => ShowId
                -> SqlPersistT m [Entity Episode]
getShowEpisodes showKey = rawSql _sql _params
    where
        _sql = [st|
                    select ??
                    from show
                        join season on (season.show = show.id)
                        join episode on (episode.season = season.id)
                    where
                        show.id = ?
                    order by season.number, episode.number
            |]
        _params = [toPersistValue showKey]


getShowSeasons :: MonadIO m
               => ShowId
               -> SqlPersistT m [Entity Season]
getShowSeasons showKey = rawSql _sql _params
    where
        _sql = [st|
                    select ??
                    from season
                    where season.show = ?
                    order by season.number
            |]
        _params = [toPersistValue showKey]


updateShowLastUpdate :: MonadIO m
                     => ShowId
                     -> UTCTime
                     -> SqlPersistT m ()
updateShowLastUpdate showKey time = update showKey [ShowLastUpdate =. Just time]


updateShowNextUpdate :: MonadIO m
                     => ShowId
                     -> UTCTime
                     -> SqlPersistT m ()
updateShowNextUpdate showKey time = update showKey [ShowNextUpdate =. Just time]


-- | Delete episode and data that references given episode.
-- episodeCode is a tuple of Integrals which are converted to Ints in impl - I'd prefer to expose
-- generic interface. If we're outside of range, then we'll fail.
deleteEpisodeByEpisodeCode :: (MonadIO m, MonadLogger m, Integral a, Integral b)
                           => ShowId
                           -> (a, b)
                           -> SqlPersistT m ()
deleteEpisodeByEpisodeCode showId episodeCode = do
    $(logDebug) "Deleting episode…"
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


updateSeason :: MonadIO m => UTCTime -> ShowId -> Int -> Int -> SqlPersistT m ()
updateSeason _now _showId _seasonNumber _newSeasonNumber = do
    update _showId [ShowModified =. _now]
    ms <- getBy $ UniqueShowSeasonNumber _showId _seasonNumber
    case ms of
        Just (Entity _seasonId _) -> update _seasonId [SeasonModified =. _now, SeasonNumber =. _newSeasonNumber]
        Nothing -> return ()

