
module Handler.Admin (
    getAdminR
) where


import Prelude hiding (Show, show)
import Yesod
import Yesod.Auth (requireAuth)

import Episodes.Common (formatEpisodeCode, formatTime)
import Episodes.DB (getRecentEpisodeStatuses)
import Foundation
import Model
import Settings (widgetFile)


getAdminR :: Handler Html
getAdminR = do
    a <- requireAuth
    if accountAdmin $ entityVal a then do
        episode_statuses <- runDB $ getRecentEpisodeStatuses 512
        defaultLayout $ do
            setTitle "Admin"
            $(widgetFile "admin")
    else notFound

