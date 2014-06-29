{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Users where

import Import


getProfileR :: Handler Html
getProfileR = do
    defaultLayout $ do
        $(widgetFile "profile")


--getHomeR :: Handler Html
--getHomeR = do
--    (formWidget, formEnctype) <- generateFormPost sampleForm
--    let submission = Nothing :: Maybe (FileInfo, Text)
--        handlerName = "getHomeR" :: Text
--    defaultLayout $ do
--        aDomId <- newIdent
--        setTitle "Welcome To Yesod!"
--        $(widgetFile "homepage")

--postHomeR :: Handler Html
--postHomeR = do
--    ((result, formWidget), formEnctype) <- runFormPost sampleForm
--    let handlerName = "postHomeR" :: Text
--        submission = case result of
--            FormSuccess res -> Just res
--            _ -> Nothing

--    defaultLayout $ do
--        aDomId <- newIdent
--        setTitle "Welcome To Yesod!"
--        $(widgetFile "homepage")

--sampleForm :: Form (FileInfo, Text)
--sampleForm = renderDivs $ (,)
--    <$> fileAFormReq "Choose a file"
--    <*> areq textField "What's on the file?" Nothing
