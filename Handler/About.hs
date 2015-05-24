
module Handler.About (
    getAboutR
) where


import Prelude hiding (Show, show)
import Yesod (Html, defaultLayout, setTitle, runDB)

import Foundation
import Settings (widgetFile)


getAboutR :: Handler Html
getAboutR = do
    defaultLayout $ do
        setTitle "About"
        $(widgetFile "about")