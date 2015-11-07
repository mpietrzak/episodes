
module Episodes.DB.ShowChanges (
    addShowChangeAddSeasons,
    addShowChangeEditSeason,
    addShowChangeDeleteSeason,
    addShowChangeAddEpisodes,
    addShowChangeEditEpisode,
    addShowChangeDeleteEpisode,
    createShowChange,
    getAddSeasonChanges
) where


import Prelude
import Control.Monad.IO.Class (MonadIO)
import Database.Persist (insert, insert_)
import Database.Persist.Sql ((==.), Entity, SqlPersistT, selectList)
import Data.Time (UTCTime)
import Data.Text (Text)

import Model ( AccountId
             , EpisodeId
             , SeasonId
             , ShowChange ( ShowChange
                          , showChangeShow
                          , showChangeAuthor
                          , showChangeCreated
                          , showChangeModified )
             , ShowChangeId
             , ShowChangeAddEpisodes ( ShowChangeAddEpisodes
                                     , showChangeAddEpisodesChange
                                     , showChangeAddEpisodesSeason
                                     , showChangeAddEpisodesCount
                                     , showChangeAddEpisodesCreated
                                     , showChangeAddEpisodesModified )
             , ShowChangeAddSeasons ( ShowChangeAddSeasons
                                    , showChangeAddSeasonsChange
                                    , showChangeAddSeasonsCount
                                    , showChangeAddSeasonsCreated
                                    , showChangeAddSeasonsModified )
             , ShowChangeDeleteSeason ( ShowChangeDeleteSeason
                                      , showChangeDeleteSeasonChange
                                      , showChangeDeleteSeasonSeason
                                      , showChangeDeleteSeasonCreated
                                      , showChangeDeleteSeasonModified )
             , ShowChangeDeleteEpisode ( ShowChangeDeleteEpisode
                                       , showChangeDeleteEpisodeChange
                                       , showChangeDeleteEpisodeEpisode
                                       , showChangeDeleteEpisodeCreated
                                       , showChangeDeleteEpisodeModified )
             , ShowChangeEditEpisode ( ShowChangeEditEpisode
                                     , showChangeEditEpisodeChange
                                     , showChangeEditEpisodeEpisode
                                     , showChangeEditEpisodeTitle
                                     , showChangeEditEpisodeNumber
                                     , showChangeEditEpisodeCreated
                                     , showChangeEditEpisodeModified )
             , ShowChangeEditSeason ( ShowChangeEditSeason
                                    , showChangeEditSeasonChange
                                    , showChangeEditSeasonNumber
                                    , showChangeEditSeasonCreated
                                    , showChangeEditSeasonModified )
             , ShowId )
import qualified Model as M

addShowChangeAddSeasons :: MonadIO m => UTCTime -> ShowChangeId -> Int -> SqlPersistT m ()
addShowChangeAddSeasons _now _showChangeId _count =
    insert_ ShowChangeAddSeasons { showChangeAddSeasonsChange = _showChangeId
                                 , showChangeAddSeasonsCount = _count
                                 , showChangeAddSeasonsCreated = _now
                                 , showChangeAddSeasonsModified = _now }


getAddSeasonChanges :: MonadIO m => ShowChangeId -> SqlPersistT m [Entity ShowChangeAddSeasons]
getAddSeasonChanges _showChangeId = selectList [M.ShowChangeAddSeasonsChange ==. _showChangeId] []


addShowChangeEditSeason :: MonadIO m => UTCTime -> ShowChangeId -> Int -> SqlPersistT m ()
addShowChangeEditSeason _now _showChangeId _num =
    insert_ ShowChangeEditSeason { showChangeEditSeasonChange = _showChangeId
                                 , showChangeEditSeasonNumber = _num
                                 , showChangeEditSeasonCreated = _now
                                 , showChangeEditSeasonModified = _now }


addShowChangeDeleteSeason :: MonadIO m => UTCTime -> ShowChangeId -> SeasonId -> SqlPersistT m ()
addShowChangeDeleteSeason _now _showChangeId _seasonId =
    insert_ ShowChangeDeleteSeason { showChangeDeleteSeasonChange = _showChangeId
                                   , showChangeDeleteSeasonSeason = _seasonId
                                   , showChangeDeleteSeasonCreated = _now
                                   , showChangeDeleteSeasonModified = _now }


addShowChangeAddEpisodes :: MonadIO m => UTCTime -> ShowChangeId -> SeasonId -> Int -> SqlPersistT m ()
addShowChangeAddEpisodes _now _showChangeId _seasonId _count =
    insert_ ShowChangeAddEpisodes { showChangeAddEpisodesChange = _showChangeId
                                  , showChangeAddEpisodesSeason = _seasonId
                                  , showChangeAddEpisodesCount = _count
                                  , showChangeAddEpisodesCreated = _now
                                  , showChangeAddEpisodesModified = _now }


addShowChangeEditEpisode :: MonadIO m => UTCTime -> ShowChangeId -> EpisodeId -> Maybe Text -> Maybe Int -> SqlPersistT m ()
addShowChangeEditEpisode _now _showChangeId _episodeId _mtitle _mnumber =
    insert_ ShowChangeEditEpisode { showChangeEditEpisodeChange = _showChangeId
                                  , showChangeEditEpisodeEpisode = _episodeId
                                  , showChangeEditEpisodeTitle = _mtitle
                                  , showChangeEditEpisodeNumber = _mnumber
                                  , showChangeEditEpisodeCreated = _now
                                  , showChangeEditEpisodeModified = _now }


addShowChangeDeleteEpisode :: MonadIO m => UTCTime -> ShowChangeId -> EpisodeId -> SqlPersistT m ()
addShowChangeDeleteEpisode _now _showChangeId _episodeId =
    insert_ ShowChangeDeleteEpisode { showChangeDeleteEpisodeChange = _showChangeId
                                    , showChangeDeleteEpisodeEpisode = _episodeId
                                    , showChangeDeleteEpisodeCreated = _now
                                    , showChangeDeleteEpisodeModified = _now }


createShowChange :: MonadIO m => UTCTime -> AccountId -> ShowId -> SqlPersistT m ShowChangeId
createShowChange _now _accId _showId =
    insert ShowChange { showChangeShow = _showId
                      , showChangeAuthor = _accId
                      , showChangeCreated = _now
                      , showChangeModified = _now }
