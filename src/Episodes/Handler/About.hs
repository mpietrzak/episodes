{-# LANGUAGE OverloadedStrings #-}

module Episodes.Handler.About (
    getAboutR
) where


import Prelude hiding (Show, show)
import Yesod (Html, defaultLayout, setTitle)

import Foundation
import Settings (widgetFile)


getAboutR :: Handler Html
getAboutR = defaultLayout $ do
        setTitle "About"
        $(widgetFile "about")