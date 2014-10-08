{-# LANGUAGE OverloadedStrings #-}

module Episodes.Auth (
    authEpisodes
) where


import Control.Monad.Trans.Class (lift)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Prelude ( Bool (False, True)
               , IO
               , (>>=)
               , ($) )

import Yesod ( HandlerT
             , TypedContent
             , YesodDB
             , YesodPersist
             , YesodPersistBackend
             , getRouteToParent
             , hamlet
             , notFound
             , lookupPostParam
             , runDB
             , sendResponse
             , toWidget)
import Yesod.Auth ( YesodAuth
                  , Auth
                  , AuthPlugin (AuthPlugin)
                  , AuthRoute
                  , Route (LoginR, PluginR)
                  , Creds (Creds, credsPlugin, credsIdent, credsExtra)
                  , loginErrorMessage
                  , setCredsRedirect )


loginAuthRoute :: AuthRoute
loginAuthRoute = PluginR "episodes" ["login"]

postLoginR :: (YesodPersist master, YesodAuth master)
           => (Text -> Text -> YesodDB master Bool)
           -> HandlerT Auth (HandlerT master IO) TypedContent
postLoginR checkPassword = do
    tm <- getRouteToParent
    mUsername <- lookupPostParam "username"
    mPassword <- lookupPostParam "password"
    let username = fromJust mUsername
    let password = fromJust mPassword
    isPasswordOk <- lift $ runDB $ checkPassword username password
    case isPasswordOk of
        False -> lift $ loginErrorMessage (tm LoginR) "Incorrect Password"
        True -> do
            let creds = Creds { credsPlugin = "episodes"
                              , credsIdent = username
                              , credsExtra = [] }
            lift $ setCredsRedirect creds


authEpisodes :: (YesodPersist master, YesodAuth master)
             => (Text -> Text -> YesodDB master Bool)
             -> AuthPlugin master
authEpisodes checkPassword = AuthPlugin name dispatch widget
    where
        name = "episodes"
        dispatch "POST" ["login"] = postLoginR checkPassword >>= sendResponse
        dispatch _ _ = notFound
        widget toMaster = toWidget [hamlet|
$newline always
    <div>
        <form method="post" action="@{toMaster loginAuthRoute}">
            <table>
                <tbody>
                    <tr>
                        <td>
                            Login or email:
                        <td>
                            <input type="text" name="username">
                    <tr>
                        <td>
                            Password:
                        <td>
                            <input type="password" name="password">
                    <tr>
                        <td colspan=2 align="right">
                            <button type="submit">Login
|]


