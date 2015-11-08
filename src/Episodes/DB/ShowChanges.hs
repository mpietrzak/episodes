{-# LANGUAGE QuasiQuotes #-}

module Episodes.DB.ShowChanges (
    addShowChange,
    addShowChangeDeleteEpisode,
    addShowChangeDeleteSeason,
    addShowChangeEditEpisode,
    getCurrentShowChange,
    getDeleteEpisodeChanges,
    getDeleteSeasonChanges,
    getEditEpisodeChanges,
    getShowChangesForAccept,
    submitShowChange
) where


import Prelude
import Control.Monad.IO.Class (MonadIO)
import Database.Persist (SelectOpt(LimitTo), entityKey, entityVal, getBy, insert, insert_, insertEntity, selectFirst, update)
import Database.Persist.Sql ((=.), (==.), Entity, SqlPersistT, rawSql, selectList)
import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.Time (UTCTime)
import Data.Text (Text)
import Formatting ((%), sformat, text)
import Text.Shakespeare.Text (st)
import qualified Data.Map.Strict as M

import Model ( Account
             , AccountId
             , EpisodeId
             , SeasonId
             , ShowChange ( ShowChange
                          , showChangeShow
                          , showChangeAuthor
                          , showChangeSubmitted
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


addShowChangeEditEpisode :: MonadIO m => UTCTime -> ShowChangeId -> Int -> Int -> Text -> SqlPersistT m ()
addShowChangeEditEpisode _now _showChangeId _seasonNumber _episodeNumber _title =
    insert_ M.ShowChangeEditEpisode { M.showChangeEditEpisodeChange = _showChangeId
                                    , M.showChangeEditEpisodeTitle = _title
                                    , M.showChangeEditEpisodeSeasonNumber = _seasonNumber
                                    , M.showChangeEditEpisodeEpisodeNumber = _episodeNumber
                                    , M.showChangeEditEpisodeCreated = _now
                                    , M.showChangeEditEpisodeModified = _now }


addShowChange :: MonadIO m => UTCTime -> AccountId -> ShowId -> SqlPersistT m (Entity ShowChange)
addShowChange _now _accId _showId =
    insertEntity ShowChange { showChangeShow = _showId
                            , showChangeAuthor = _accId
                            , showChangeSubmitted = False
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
                                                                            , [ Entity ShowChangeDeleteSeason ]
                                                                            , [ Entity ShowChangeDeleteEpisode ]
                                                                            , [ Entity ShowChangeEditEpisode ] ) ]
getShowChangesForAccept _accountId _count = do
        -- TODO: don't use maps :<
        -- entities
        _dses <- rawSql (_sql "show_change_delete_season") []
        _dees <- rawSql (_sql "show_change_delete_episode") []
        _eees <- rawSql (_sql "show_change_edit_episode") []
        -- maps :<
        let _mds = groupMap _dses (showChangeDeleteSeasonChange . entityVal)
        let _mde = groupMap _dees (showChangeDeleteEpisodeChange . entityVal)
        let _mee = groupMap _eees (showChangeEditEpisodeChange . entityVal)
        -- change with author entities
        _chaes <- rawSql _sqlChanges []
        -- zip it using maps
        return $ map (\(_che, _ae) -> ( _che
                                      , _ae
                                      , M.findWithDefault [] (entityKey _che) _mds
                                      , M.findWithDefault [] (entityKey _che) _mde
                                      , M.findWithDefault [] (entityKey _che) _mee ))
                     _chaes
    where
        -- sql to select [(change, author)]
        _sqlChanges = [st|
            select ??, ??
            from show_change join account on (show_change.author = account.id)
            where
                show_change.submitted = true
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
