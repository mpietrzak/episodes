


module Handler.Stats (
    getStatsR
) where


import Prelude

import Yesod (Html, defaultLayout, setTitle)

import Foundation (Handler)
import Settings (widgetFile)


getStatsR :: Handler Html
getStatsR = do
    defaultLayout $ do
        setTitle "Stats"
        $(widgetFile "stats")

