

module Episodes.Handler.Stats (
    getStatsR
) where


import Prelude hiding (Show, show)

import Yesod (Html, defaultLayout, setTitle, runDB)
import Database.Persist

import Foundation
import Settings (widgetFile)

import Model
import Episodes.Common (formatEpisodeCode)
import Episodes.DB (getPopularEpisodes,
                    getPopularShows,
                    getRecentlyPopularEpisodes)


getStatsR :: Handler Html
getStatsR = do
    popularShows <- runDB $ getPopularShows 32
    popularEpisodes <- runDB $ getPopularEpisodes (32 :: Int)
    recentlyPopularEpisodes <- runDB $ getRecentlyPopularEpisodes 256 32
    defaultLayout $ do
        setTitle "Stats"
        $(widgetFile "stats")

