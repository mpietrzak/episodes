module Episodes.Cache (
    -- defaultCache
) where


import Data.Default (def)
import Data.Time (UTCTime)
import Database.Persist (Entity)
import qualified Data.HashMap.Strict as M


import Model


data EpisodesCache = EpisodesCache {
        ecShowEpisodes :: M.HashMap ShowId (CachedValue [Entity Episode])
    }


data CachedValue a = CachedValue {
        cvValue :: a,
        cvExpires :: UTCTime
    }


