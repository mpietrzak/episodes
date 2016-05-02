{-# LANGUAGE QuasiQuotes #-}

module Episodes.DB.ShowChanges (
    acceptShowChange,
    addShowChange,
    addShowChangeDeleteEpisode,
    addShowChangeDeleteSeason,
    addShowChangeEditEpisode,
    addShowChangePublishShow,
    getCurrentShowChange,
    getDeleteEpisodeChanges,
    getDeleteSeasonChanges,
    getEditEpisodeChanges,
    getShowChangesForAccept,
    rejectShowChange,
    submitShowChange
) where


import Prelude hiding (Show)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger(..))
import Database.Persist (SelectOpt(LimitTo), entityKey, entityVal, get, getBy, insert, insert_, insertEntity, toPersistValue, selectFirst, update)
import Database.Persist.Sql ((=.), (==.), Entity(Entity), SqlPersistT, fromSqlKey, rawExecute, rawSql, selectList)
import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.Monoid ((<>))
import Data.Time (UTCTime)
import Data.Text (Text)
import Formatting ((%), int, sformat, text)
import Text.Shakespeare.Text (st)
import Yesod.Core (logDebug)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set

import Model ( AccountId
             , Episode(Episode)
             , EpisodeId
             , SeasonId
             , ShowChange ( ShowChange
                          , showChangeShow
                          , showChangeAuthor
                          , showChangeSubmitted
                          , showChangeAccepted
                          , showChangeRejected
                          , showChangeCreated
                          , showChangeModified )
             , ShowChangeId
             , ShowChangeDeleteSeason ( ShowChangeDeleteSeason
                                      , showChangeDeleteSeasonChange
                                      , showChangeDeleteSeasonSeasonNumber
                                      , showChangeDeleteSeasonCreated
                                      , showChangeDeleteSeasonModified )
             , ShowChangeDeleteEpisode ( ShowChangeDeleteEpisode
                                       , showChangeDeleteEpisodeChange
                                       , showChangeDeleteEpisodeSeasonNumber
                                       , showChangeDeleteEpisodeEpisodeNumber
                                       , showChangeDeleteEpisodeCreated
                                       , showChangeDeleteEpisodeModified )
             , ShowChangeEditEpisode ( ShowChangeEditEpisode
                                     , showChangeEditEpisodeChange
                                     , showChangeEditEpisodeTitle
                                     , showChangeEditEpisodeSeasonNumber
                                     , showChangeEditEpisodeEpisodeNumber
                                     , showChangeEditEpisodeAirDateTime
                                     , showChangeEditEpisodeCreated
                                     , showChangeEditEpisodeModified )
             , ShowId )
import qualified Model as M


addShowChangeDeleteEpisode :: MonadIO m => UTCTime -> ShowChangeId -> Int -> Int -> SqlPersistT m ()
addShowChangeDeleteEpisode _now _showChangeId _seasonNumber _episodeNumber =
    insert_ ShowChangeDeleteEpisode { showChangeDeleteEpisodeChange = _showChangeId
                                    , showChangeDeleteEpisodeSeasonNumber = _seasonNumber
                                    , showChangeDeleteEpisodeEpisodeNumber = _episodeNumber
                                    , showChangeDeleteEpisodeCreated = _now
                                    , showChangeDeleteEpisodeModified = _now }


addShowChangeDeleteSeason :: MonadIO m => UTCTime -> ShowChangeId -> Int -> SqlPersistT m ()
addShowChangeDeleteSeason _now _showChangeId _seasonNumber =
    insert_ ShowChangeDeleteSeason { showChangeDeleteSeasonChange = _showChangeId
                                   , showChangeDeleteSeasonSeasonNumber = _seasonNumber
                                   , showChangeDeleteSeasonCreated = _now
                                   , showChangeDeleteSeasonModified = _now }


addShowChangeEditEpisode :: MonadIO m => UTCTime -> ShowChangeId -> Int -> Int -> Text -> UTCTime -> SqlPersistT m ()
addShowChangeEditEpisode _now _showChangeId _seasonNumber _episodeNumber _title _airDateTime =
    insert_ M.ShowChangeEditEpisode { M.showChangeEditEpisodeChange = _showChangeId
                                    , M.showChangeEditEpisodeTitle = _title
                                    , M.showChangeEditEpisodeSeasonNumber = _seasonNumber
                                    , M.showChangeEditEpisodeEpisodeNumber = _episodeNumber
                                    , M.showChangeEditEpisodeAirDateTime = _airDateTime
                                    , M.showChangeEditEpisodeCreated = _now
                                    , M.showChangeEditEpisodeModified = _now }


addShowChange :: MonadIO m => UTCTime -> AccountId -> ShowId -> SqlPersistT m (Entity ShowChange)
addShowChange _now _accId _showId =
    insertEntity ShowChange { showChangeShow = _showId
                            , showChangeAuthor = _accId
                            , showChangeSubmitted = False
                            , showChangeAccepted = False
                            , showChangeRejected = False
                            , showChangeCreated = _now
                            , showChangeModified = _now }


addShowChangePublishShow :: MonadIO m => UTCTime -> M.AccountId -> M.ShowId -> SqlPersistT m ()
addShowChangePublishShow _now _accountId _showId = do
    update _showId [M.ShowSubmitted =. True, M.ShowModified =. _now]
    _showChangeKey <- insert M.ShowChange { M.showChangeShow = _showId
                                          , M.showChangeAuthor = _accountId
                                          , M.showChangeSubmitted = True
                                          , M.showChangeAccepted = False
                                          , M.showChangeRejected = False
                                          , M.showChangeCreated = _now
                                          , M.showChangeModified = _now }
    insert_ M.ShowChangePublishShow { M.showChangePublishShowChange = _showChangeKey
                                    , M.showChangePublishShowCreated = _now
                                    , M.showChangePublishShowModified = _now }


getEditEpisodeChanges :: MonadIO m => M.ShowChangeId -> SqlPersistT m [Entity M.ShowChangeEditEpisode]
getEditEpisodeChanges _showChangeId = selectList [M.ShowChangeEditEpisodeChange ==. _showChangeId] []


getDeleteEpisodeChanges :: MonadIO m => M.ShowChangeId -> SqlPersistT m [Entity M.ShowChangeDeleteEpisode]
getDeleteEpisodeChanges _showChangeId = selectList [M.ShowChangeDeleteEpisodeChange ==. _showChangeId] []


getDeleteSeasonChanges :: MonadIO m => M.ShowChangeId -> SqlPersistT m [Entity M.ShowChangeDeleteSeason]
getDeleteSeasonChanges _showChangeId = selectList [M.ShowChangeDeleteSeasonChange ==. _showChangeId] []


getCurrentShowChange :: MonadIO m => AccountId -> ShowId -> SqlPersistT m (Maybe (Entity ShowChange))
getCurrentShowChange _accountId _showId = selectFirst [ M.ShowChangeAuthor ==. _accountId
                                                      , M.ShowChangeShow ==. _showId
                                                      , M.ShowChangeSubmitted ==. False ]
                                                      []


groupMap :: Ord b => forall a. [a] -> (a -> b) -> M.Map b [a]
groupMap l kf = M.fromList _tups
    where
        _ll = groupBy _gf $ sortBy _sf l
        -- sortBy predicate
        _sf = compare `on` kf
        -- groupBy predicate
        _gf = (==) `on` kf
        -- list of tuples fed to fromList: (key, value); key is resulf of kf and value is list of items with given kf
        _tups = map _l2t _ll
        -- make sublist into tuple, assumes _l not empty (groupBy returns nonempty lists)
        _l2t _l = (kf (head _l), _l)


getShowChangesForAccept :: MonadIO m => M.AccountId -> Int -> SqlPersistT m [ ( Entity M.ShowChange
                                                                             , Entity M.Account
                                                                             , Entity M.Show
                                                                             , [ Entity M.ShowChangePublishShow ]
                                                                             , [ Entity M.ShowChangeDeleteSeason ]
                                                                             , [ Entity M.ShowChangeDeleteEpisode ]
                                                                             , [ Entity M.ShowChangeEditEpisode ] ) ]
getShowChangesForAccept _accountId _count = do
        -- TODO: don't use maps :<
        -- we could get data in sorted form from db, then we could zip it in O(n) (one pass over those lists)
        -- but data size is so small that it does not make practical sense at this point to sort more.

        -- 1. entities
        _pses <- rawSql (_sql "show_change_publish_show") []
        _dses <- rawSql (_sql "show_change_delete_season") []
        _dees <- rawSql (_sql "show_change_delete_episode") []
        _eees <- rawSql (_sql "show_change_edit_episode") []
        -- 2. maps :<
        let _mps = groupMap _pses (M.showChangePublishShowChange . entityVal)
        let _mds = groupMap _dses (M.showChangeDeleteSeasonChange . entityVal)
        let _mde = groupMap _dees (M.showChangeDeleteEpisodeChange . entityVal)
        let _mee = groupMap _eees (M.showChangeEditEpisodeChange . entityVal)
        -- 3. changes with author and show entities
        _chaes <- rawSql _sqlChanges []
        -- 4. finally zip them using maps
        return $ map (\(_che, _ae, _se) -> ( _che -- change entity
                                           , _ae  -- account entity
                                           , _se  -- show entity
                                           , M.findWithDefault [] (entityKey _che) _mps -- publish show
                                           , M.findWithDefault [] (entityKey _che) _mds -- delete season
                                           , M.findWithDefault [] (entityKey _che) _mde -- delete episode
                                           , M.findWithDefault [] (entityKey _che) _mee -- edit episode
                                           ))
                     _chaes
    where
        -- sql to select [(change, author, show)]
        _sqlChanges = [st|
            select ??, ??, ??
            from
              show_change
              join account on (show_change.author = account.id)
              join show on (show_change.show = show.id)
            where
                show_change.submitted = true
                and show_change.accepted = false
                and show_change.rejected = false
            |]
        -- sql to select change element (deletes, edits, ...)
        _sql _table = sformat
          (
              "select ?? from show_change join " % text %
              " on (" % text % ".change = show_change.id)" %
              " where show_change.submitted = true" %
              " order by show_change.id"
          )
          _table _table


-- | Mark show change as submitted.
submitShowChange :: MonadIO m => UTCTime -> ShowChangeId -> SqlPersistT m ()
submitShowChange _now _showChangeId = update _showChangeId [ M.ShowChangeSubmitted =. True
                                                           , M.ShowChangeModified =. _now ]


_acceptShowChangePublishShow :: MonadIO m => M.ShowId -> SqlPersistT m ()
_acceptShowChangePublishShow _showId = update _showId [ M.ShowPublic =. True ]


-- | Delete episodes that belong to given change.
-- Executed by acceptShowChange.
_acceptShowChangeDeleteEpisodes :: MonadIO m => ShowChangeId -> SqlPersistT m ()
_acceptShowChangeDeleteEpisodes _changeId = do
        rawExecute _subscriptionsNextEpSql _params
        rawExecute _subscriptionsLastEpSql _params
        rawExecute _deleteEpisodesSql _params
    where
        _subselectEpisodeIds = [st|
            select
                episode.id
            from
                show
                join season on (season.show = show.id)
                join episode on (episode.season = season.id)
                join show_change on (show_change.show = show.id)
                join show_change_delete_episode on (
                    show_change_delete_episode.change = show_change.id
                    and show_change_delete_episode.season_number = season.number
                    and show_change_delete_episode.episode_number = episode.number)
            where
                show_change.id = ?
            |]
        _subscriptionsNextEpSql = "update subscription set next_episode = null where next_episode in (" <> _subselectEpisodeIds <> ")"
        _subscriptionsLastEpSql = "update subscription set last_episode = null where last_episode in (" <> _subselectEpisodeIds <> ")"
        _deleteEpisodesSql = "delete from episode where id in (" <> _subselectEpisodeIds <> ")"
        _params = [toPersistValue _changeId]


-- | Create missing seasons if needed by change.
-- Return map of season number to season entity of seasons referenced by edit-episode changes (may be not all seasons).
_acceptShowChangeUpsertSeasons :: (MonadIO m, MonadLogger m)
                               => UTCTime
                               -> ShowChangeId
                               -> ShowId
                               -> [Entity M.ShowChangeEditEpisode]
                               -> SqlPersistT m (M.Map Int (Entity M.Season))
_acceptShowChangeUpsertSeasons _now showChangeId showId editEpisodeEntities = do
    let _epSeasonSet = Set.fromList $ map (showChangeEditEpisodeSeasonNumber . entityVal) editEpisodeEntities
    _seasonL <- forM (Set.toList _epSeasonSet) $ \_sn -> do
        -- sn is season number; first try to get; if not found then insert; finally return pair
        _ms <- selectFirst [ M.SeasonShow ==. showId
                           , M.SeasonNumber ==. _sn ] []
        case _ms of
            Just _se -> do
                $(logDebug) $ sformat ("season " % int % " already exists") _sn
                return (_sn, _se)
            Nothing -> do
                $(logDebug) $ sformat ("season " % int % " will be inserted") _sn
                let _s = M.Season { M.seasonShow = showId
                                  , M.seasonNumber = _sn
                                  , M.seasonCreated = _now
                                  , M.seasonModified = _now }
                _i <- insert _s
                return (_sn, Entity _i _s)
    -- so not seasonL is list of pairs: (seasonNumber, seasonEntity) -> let's build map
    return $ M.fromList _seasonL


_acceptShowChangeEditEpisodes :: (MonadIO m, MonadLogger m) => UTCTime -> ShowChangeId -> ShowId -> SqlPersistT m ()
_acceptShowChangeEditEpisodes _now _showChangeId _showId = do
    _eees <- selectList [M.ShowChangeEditEpisodeChange ==. _showChangeId] []
    _seasonsMap <- _acceptShowChangeUpsertSeasons _now _showChangeId _showId _eees
    -- now _seasonsMap is map of seasonNumber -> seasonEntity for seasons that are refernced by episode edits
    -- for each episode in edit-episode entity list: we update or insert episode
    forM_ _eees $ \(Entity _eeId _ee) -> do
        $(logDebug) $ sformat ("processing edit " % int % "/" % int)
                              (M.showChangeEditEpisodeSeasonNumber _ee)
                              (M.showChangeEditEpisodeEpisodeNumber _ee)
        let _mSeason = M.lookup (showChangeEditEpisodeSeasonNumber _ee) _seasonsMap
        $(logDebug) $ case _mSeason of
            Just _ -> sformat "season for this episode already exists"
            Nothing -> sformat "season for this episode does not exist: should have been created by upsert seasons…"
        _mEpisode <- case _mSeason of
                Nothing -> return Nothing -- shouldn't happen… is it ok to error here?
                Just (Entity _seasonId _season) -> selectFirst [ M.EpisodeSeason ==. _seasonId
                                                               , M.EpisodeNumber ==. showChangeEditEpisodeEpisodeNumber _ee ]
                                                               []
        case _mEpisode of
            Just (Entity _episodeId _episode) ->
                update _episodeId [ M.EpisodeTitle =. showChangeEditEpisodeTitle _ee
                                  , M.EpisodeModified =. _now ]
            Nothing -> do
                let _n = showChangeEditEpisodeSeasonNumber _ee
                let _mSeasonEnt = M.lookup _n _seasonsMap
                case _mSeasonEnt of
                    Nothing -> return () -- TODO: shouldn't happen!
                    Just (Entity _seasonId _season) ->
                        insert_ Episode { M.episodeTitle = showChangeEditEpisodeTitle _ee
                                        , M.episodeNumber = showChangeEditEpisodeEpisodeNumber _ee
                                        , M.episodeSeason = _seasonId
                                        , M.episodeAirDateTime = Just (showChangeEditEpisodeAirDateTime _ee)
                                        , M.episodeViewCount = 0
                                        , M.episodeCreated = _now
                                        , M.episodeModified = _now }


-- | Accept change: delete seasons (including episodes).
_acceptShowChangeDeleteSeasons :: (MonadIO m, MonadLogger m) => ShowChangeId -> SqlPersistT m ()
_acceptShowChangeDeleteSeasons _showChangeId = do
        rawExecute _sqlDeleteEps [toPersistValue _showChangeId]
        rawExecute _sqlDeleteSeasons [toPersistValue _showChangeId]
    where
        _sqlDeleteEps = [st|
            delete
            from episode
            where id in (
                select
                    episode.id
                from
                    show_change
                    join show_change_delete_season on (show_change_delete_season.change = show_change.id)
                    join season on (
                        season.show = show_change.show
                        and season.number = show_change_delete_season.season_number
                    )
                    join episode on (episode.season = season.id)
                where
                    show_change.id = ?
            )
        |]
        _sqlDeleteSeasons = [st|
            delete from season
            where id in (
                select season.id
                from
                    show_change
                    join show_change_delete_season on (show_change_delete_season.change = show_change.id)
                    join season on (
                        season.show = show_change.show
                        and season.number = show_change_delete_season.season_number
                    )
                where
                    show_change.id = ?
            )
        |]

-- | Accept change: modify show and mark change as accepted.
acceptShowChange :: (MonadIO m, MonadLogger m) => UTCTime -> ShowChangeId -> SqlPersistT m ()
acceptShowChange _now _showChangeId = do
    $(logDebug) $ sformat ("accepting change " % int) (fromSqlKey _showChangeId)
    _mShowChange <- get _showChangeId
    case _mShowChange of
        Nothing -> return ()
        Just _showChange -> do
            let _showId = showChangeShow _showChange
            _mShow <- get _showId
            case _mShow of
                Nothing -> return ()
                Just _show -> do
                    update _showId [ M.ShowModified =. _now
                                   , M.ShowLocal =. True ]
                    update _showChangeId [ M.ShowChangeAccepted =. True
                                         , M.ShowChangeModified =. _now ]
                    _acceptShowChangePublishShow _showId
                    _acceptShowChangeEditEpisodes _now _showChangeId _showId
                    _acceptShowChangeDeleteEpisodes _showChangeId
                    _acceptShowChangeDeleteSeasons _showChangeId


rejectShowChange :: MonadIO m => UTCTime -> ShowChangeId -> SqlPersistT m ()
rejectShowChange _now _showChangeId = update _showChangeId [ M.ShowChangeRejected =. True
                                                           , M.ShowChangeModified =. _now ]




