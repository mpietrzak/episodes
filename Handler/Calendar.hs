{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Calendar where

import Import


getCalendarR :: Handler Html
getCalendarR = do
    let handlerName = "getCalendarR" :: Text
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "calendar")
