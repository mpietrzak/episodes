{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Updating data
module Episodes.Update (
    updateTVRageShows
) where


import Prelude hiding (Show)
import Control.Concurrent (forkIO)
import Control.Monad (forM_)
import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Database.Persist
import Database.Persist.Sql
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
-- import qualified Data.Time as DT

import Model
import Episodes.Trace (traceValue)
import qualified Data.Text.IO as TIO
import qualified Episodes.DB as DB
import qualified TVRage as TVR


showCountPerUpdate :: Int
showCountPerUpdate = 128


class LogParam a where
    toText :: a -> Text


instance LogParam Text where
    toText _t = _t


instance LogParam [Text] where
    toText _ts = T.concat _ts


_log :: (LogParam a, MonadIO m) => a -> m ()
_log _lp = liftIO $ TIO.putStrLn (toText _lp)


-- | Job that updates TVRage shows, executed from time to time (eg once per day).
updateTVRageShows :: MonadIO m
                  => SqlPersistT m ()
updateTVRageShows = do
        _log ("updateTVRageShows" :: Text)

        now <- liftIO getCurrentTime

        showsToUpdateEntities <- DB.getShowsToUpdate showCountPerUpdate

        let showsToUpdate = map entityVal showsToUpdateEntities
        let showTitles = map showTitle showsToUpdate

        _log ["updating: ", T.pack $ show showTitles]

        -- for each show we...
        forM_ showsToUpdateEntities $ \showEntity -> do
            let _showKey = entityKey showEntity
            let _show = entityVal showEntity

            case showTvRageId _show of
                Nothing -> error "this show has no TVRage Id"
                Just _tvrid -> do

                    _log ["title: ", showTitle _show]
                    _log ["tv rage id: ", T.pack $ show _tvrid]

                    -- lock show by updating "last_update"
                    DB.updateShowLastUpdate _showKey now

                    -- get local data
                    localShowData <- DB.getShowData _showKey

                    -- get remote data
                    maybeRemoteShowData <- liftIO $ TVR.getFullShowInfo (toInteger _tvrid)

                    case maybeRemoteShowData of
                        Nothing -> do
                            _log ("no such show in TVR (got Nothing)" :: Text)
                        Just remoteShowData -> do
                            let changes = combineLocalAndRemoteShowData showEntity localShowData remoteShowData now
                            _log ["about to execute ", T.pack $ show (length changes), " actions"]
                            sequence_ changes
                    let nextUpdate = addUTCTime (7 * 24 * 3600) now
                    DB.updateShowNextUpdate _showKey nextUpdate
        return ()
    where
        _fork _a = liftIO $ forkIO _a


-- combineLocalAndRemoteShowData :: (PersistQuery m, PersistUnique m, MonadSqlPersist m, MonadResource m, PersistMonadBackend m ~ SqlBackend)
--                               => Entity Show
--                               -> [(Entity Show, Entity Season, Entity Episode)]
--                               -> TVR.FullShowInfo
--                               -> UTCTime
--                               -> [m ()]
combineLocalAndRemoteShowData :: MonadIO m
                              => Entity Show
                              -> [(Entity Show, Entity Season, Entity Episode)]
                              -> TVR.FullShowInfo
                              -> UTCTime
                              -> [SqlPersistT m ()]
combineLocalAndRemoteShowData _showEntity _local _remote _now = actions
    where
        actions = concat [deletes, updates, inserts]
        updates = []

        _show = entityVal _showEntity
        _showId = entityKey _showEntity

        deletes = concat [
            [_emptySeasonsDelete],
            map (DB.deleteEpisodeByEpisodeCode _showId) (HS.toList _toDeleteEpisodeCodes)]
        inserts = concat [seasonInserts, episodeInserts]

        seasonInserts = map (DB.createSeason _showId _now) (HS.toList _toInsertSeasonNumbers)
        episodeInserts = map (_episodeInsertAction _remoteEpisodeByCode) (HS.toList _toInsertEpisodeCodes)
            where
                _episodeInsertAction _map _code = DB.createEpisode _showId _seasonNumber _episodeNumber _title _airDateTime _now
                    where
                        _seasonNumber = fst _code :: Integer
                        _episodeNumber = snd _code :: Integer
                        (_, _episode) = _map HM.! _code
                        _title = TVR.episodeTitle _episode
                        _airDateTime = TVR.episodeAirDateTime _episode

        -- seasons
        _localSeasonNumbers = traceValue "local season numbers" $ HS.fromList $ map (fromIntegral . seasonNumber . entityVal . (\(_, _s, _) -> _s)) _local :: HS.HashSet Integer
        _remoteSeasonNumbers = traceValue "remote season numbers" $ HS.fromList $ map TVR.seasonNumber (TVR.fullShowInfoSeasons _remote) :: HS.HashSet Integer
        _allSeasonNumbers = HS.union _localSeasonNumbers _remoteSeasonNumbers
        _toInsertSeasonNumbers = traceValue "to insert season numbers" (HS.difference _allSeasonNumbers _localSeasonNumbers)
        _emptySeasonsDelete = DB.deleteEmptySeasons _showId

        -- episodes
        _localEpisodeCodes = HS.fromList $ map _getShowEntityRowEpisodeCode _local
        _remoteEpisodeCodes = HS.fromList $ concat $ map _fullShowInfoSeasonEpisodeCodes (TVR.fullShowInfoSeasons _remote)
        _fullShowInfoSeasonEpisodeCodes _season = map (\_e -> (TVR.seasonNumber _season, TVR.episodeNumber _e)) (TVR.seasonEpisodes _season)

        _allEpisodeCodes = HS.union _localEpisodeCodes _remoteEpisodeCodes
        _toInsertEpisodeCodes = HS.difference _allEpisodeCodes _localEpisodeCodes
        _toDeleteEpisodeCodes = HS.difference _allEpisodeCodes _remoteEpisodeCodes

        _remoteEpisodeByCode :: HM.HashMap (Integer, Integer) (TVR.Season, TVR.Episode)
        _remoteEpisodeByCode = HM.fromList $ concat $ map _toHashMapItems $ TVR.fullShowInfoSeasons _remote
            where
                -- turn a season into hash map item (k, v)
                -- wkere k is (season number, episode number)
                -- and v is (season, episode)
                _toHashMapItems _s = map (_toHashMapItem _s) $ TVR.seasonEpisodes _s
                _toHashMapItem _s _e = (_k, _v)
                    where
                        _k = (TVR.seasonNumber _s, TVR.episodeNumber _e)
                        _v = (_s, _e)

        _toUpdateEpisodes = filter _shouldUpdate _local
            where
                _shouldUpdate (_, Entity _ _season, Entity _ _episode) = _isneq (_show, _season, _episode) _remoteEpisode
                    where
                        _code = (fromIntegral $ seasonNumber _season, fromIntegral $ episodeNumber _episode) :: (Integer, Integer)
                        (_, _remoteEpisode) = _remoteEpisodeByCode HM.! _code
                        _isneq (_show, _season, _episode) _remoteEpisode = _titlesNeq || _airDateNeq
                            where
                                _titlesNeq = _localTitle /= _remoteTitle
                                _airDateNeq = _localAirDate /= _remoteAirDate
                                _localTitle = episodeTitle _episode
                                _remoteTitle = TVR.episodeTitle _remoteEpisode
                                _localAirDate = episodeAirDateTime _episode
                                _remoteAirDate = TVR.episodeAirDateTime _remoteEpisode

        -- helpers
        _getShowEntityRowEpisodeCode (_show, _season, _episode) = (
                toInteger $ seasonNumber $ entityVal _season,
                toInteger $ episodeNumber $ entityVal _episode
            )

