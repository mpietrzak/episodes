{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}

module Episodes.DB (
    getPopularShows,
    getPopularEpisodes,
    updateEpisodeViewCount,
    updateShowSubscriptionCount
) where


import Data.Text (Text)
import Database.Persist
import Database.Persist.Sql (rawSql)
import Prelude hiding (Show)
import Text.Shakespeare.Text (st)

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
    limit 32
|]


getPopularShows = selectList [] [Desc ShowSubscriptionCount, LimitTo 32]


getPopularEpisodes = rawSql selectPopularEpisodesSql []


updateEpisodeViewCount episodeId change = update episodeId [EpisodeViewCount +=. change]


updateShowSubscriptionCount showId change = update showId [ShowSubscriptionCount +=. change]