


module Handler.Stats (
    getStatsR
) where


import Prelude hiding (Show, show)

import Yesod (Html, defaultLayout, setTitle, runDB)
import Database.Persist

import Foundation
import Settings (widgetFile)

import Model
import Episodes.DB (getPopularEpisodes,
                    getPopularShows)


getStatsR :: Handler Html
getStatsR = do
    popularShows <- runDB $ getPopularShows 32
    popularEpisodes <- runDB $ getPopularEpisodes (32 :: Int)
    defaultLayout $ do
        setTitle "Stats"
        $(widgetFile "stats")

