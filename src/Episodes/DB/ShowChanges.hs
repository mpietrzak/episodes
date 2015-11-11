{-# LANGUAGE QuasiQuotes #-}

module Episodes.DB.ShowChanges (
    acceptShowChange,
    addShowChange,
    addShowChangeDeleteEpisode,
    addShowChangeDeleteSeason,
    addShowChangeEditEpisode,
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
import Database.Persist (SelectOpt(LimitTo), entityKey, entityVal, get, getBy, insert, insert_, insertEntity, selectFirst, update)
import Database.Persist.Sql ((=.), (==.), Entity(Entity), SqlPersistT, rawSql, selectList)
import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.Time (UTCTime)
import Data.Text (Text)
import Formatting ((%), sformat, text)
import Text.Shakespeare.Text (st)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set

import Model ( Account
             , AccountId
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


getShowChangesForAccept :: MonadIO m => AccountId -> Int -> SqlPersistT m [ ( Entity ShowChange
                                                                            , Entity Account
                                                                            , Entity M.Show
                                                                            , [ Entity ShowChangeDeleteSeason ]
                                                                            , [ Entity ShowChangeDeleteEpisode ]
                                                                            , [ Entity ShowChangeEditEpisode ] ) ]
getShowChangesForAccept _accountId _count = do
        -- TODO: don't use maps :<
        -- we could get data in sorted form from db, then we could zip it in O(n) (one pass over those lists)
        -- but data size is so small that it does not make practical sense at this point to sort more.

        -- 1. entities
        _dses <- rawSql (_sql "show_change_delete_season") []
        _dees <- rawSql (_sql "show_change_delete_episode") []
        _eees <- rawSql (_sql "show_change_edit_episode") []
        -- 2. maps :<
        let _mds = groupMap _dses (showChangeDeleteSeasonChange . entityVal)
        let _mde = groupMap _dees (showChangeDeleteEpisodeChange . entityVal)
        let _mee = groupMap _eees (showChangeEditEpisodeChange . entityVal)
        -- 3. changes with author and show entities
        _chaes <- rawSql _sqlChanges []
        -- 4. finally zip them using maps
        return $ map (\(_che, _ae, _se) -> ( _che -- change entity
                                           , _ae  -- account entity
                                           , _se  -- show entity
                                           , M.findWithDefault [] (entityKey _che) _mds
                                           , M.findWithDefault [] (entityKey _che) _mde
                                           , M.findWithDefault [] (entityKey _che) _mee ))
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
        -- sql to select change element (deletes, edits)
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


-- | Accept change: modify show and mark change as accepted.
acceptShowChange :: MonadIO m => UTCTime -> ShowChangeId -> SqlPersistT m ()
acceptShowChange _now _showChangeId = do
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
                                   , M.ShowLocal =. Just True ]
                    update _showChangeId [ M.ShowChangeAccepted =. True
                                         , M.ShowChangeModified =. _now ]
                    -- 1. edit episodes
                    _eees <- selectList [M.ShowChangeEditEpisodeChange ==. _showChangeId] []
                    -- 1.2 create seasons that don't yet exist, return map of season num -> season entity
                    _seasonMap <- do
                        let _epSeasonSet = Set.fromList $ map (showChangeEditEpisodeSeasonNumber . entityVal) _eees
                        _seasonL <- forM (Set.toList _epSeasonSet) $ \_sn -> do
                            -- sn is season number; first try to get; if not found - insert; return pair
                            _ms <- selectFirst [ M.SeasonShow ==. _showId
                                               , M.SeasonNumber ==. _sn ] []
                            case _ms of
                                Just _se -> return (_sn, _se)
                                Nothing -> do
                                    let _s = M.Season { M.seasonShow = _showId
                                                      , M.seasonNumber = _sn
                                                      , M.seasonCreated = _now
                                                      , M.seasonModified = _now }
                                    _i <- insert _s
                                    return (_sn, Entity _i _s)
                        -- so not seasonL is list of pairs: (seasonNumber, seasonEntity) -> let's build map
                        return $ M.fromList _seasonL
                    -- now _seasonsMap is map of seasonNumber -> seasonEntity for seasons that are refernced by episode edits
                    forM_ _eees $ \(Entity _eeId _ee) -> do
                        _mSeason <- selectFirst [ M.SeasonShow ==. _showId
                                                , M.SeasonNumber ==. showChangeEditEpisodeSeasonNumber _ee ]
                                                []
                        mEpisode <- case _mSeason of
                                Nothing -> return Nothing
                                Just (Entity _seasonId _season) -> selectFirst [ M.EpisodeSeason ==. _seasonId
                                                                               , M.EpisodeNumber ==. showChangeEditEpisodeEpisodeNumber _ee ]
                                                                               []
                        case mEpisode of
                            Just (Entity _episodeId _episode) ->
                                update _episodeId [ M.EpisodeTitle =. showChangeEditEpisodeTitle _ee
                                                  , M.EpisodeModified =. _now ]
                            Nothing -> do
                                let _n = showChangeEditEpisodeSeasonNumber _ee
                                let _mSeasonEnt = M.lookup _n _seasonMap
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


rejectShowChange :: MonadIO m => UTCTime -> ShowChangeId -> SqlPersistT m ()
rejectShowChange _now _showChangeId = update _showChangeId [ M.ShowChangeRejected =. True
                                                           , M.ShowChangeModified =. _now ]




