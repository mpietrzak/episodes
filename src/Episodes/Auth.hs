{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Episodes.Auth (
    authEpisodes
) where


import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(Just), fromJust)
import Data.Text (Text)
import Prelude ( Bool (False, True)
               , IO
               , (>>=)
               , ($) )

import Yesod ( HandlerT
             , TypedContent
             , YesodDB
             , YesodPersist
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


registerAuthRoute :: AuthRoute
registerAuthRoute = PluginR "episodes" ["register"]


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


postRegisterR :: YesodAuth master
              => (Text -> Text -> HandlerT master IO Bool)
              -> HandlerT Auth (HandlerT master IO) TypedContent
postRegisterR createUser = do
    tm <- getRouteToParent
    mUsername <- lookupPostParam "username"
    mPassword <- lookupPostParam "password"
    case (mUsername, mPassword) of
        (Just username, Just password) -> do
            insertOk <- lift $ createUser username password
            if insertOk
                then do
                    let creds = Creds { credsPlugin = "episodes"
                                      , credsIdent = username
                                      , credsExtra = [] }
                    lift $ setCredsRedirect creds
                else lift $ loginErrorMessage (tm LoginR) "Failed to create user"
        _ -> lift $ loginErrorMessage (tm LoginR) "Invalid Params"


authEpisodes :: (YesodPersist master, YesodAuth master)
             => (Text -> Text -> HandlerT master IO Bool)
             -> (Text -> Text -> YesodDB master Bool)
             -> AuthPlugin master
authEpisodes createUser checkPassword = AuthPlugin name dispatch widget
    where
        name = "episodes"
        dispatch "POST" ["login"] = postLoginR checkPassword >>= sendResponse
        dispatch "POST" ["register"] = postRegisterR createUser >>= sendResponse
        dispatch _ _ = notFound
        widget toMaster = toWidget [hamlet|
$newline always
    <div class="container">
        <div class="col-sm-6">
            <h5>Register
            <form class="form-horizontal" method="post" action="@{toMaster registerAuthRoute}">
                <div class="form-group">
                    <label for="register-username" class="col-sm-4 control-label">Login
                    <div class="col-sm-8">
                        <input type="text" class="form-control" id="register-username" name="username" placeholder="Login">
                <div class="form-group">
                    <label for="register-password" class="col-sm-4 control-label">Password
                    <div class="col-sm-8">
                        <input type="password" class="form-control" id="regitser-password" name="password" placeholder="Password">
                <div class="form-group">
                    <div class="col-sm-12" style="text-align: right">
                      <button type="submit" class="btn btn-default">Register
        <div class="col-sm-6">
            <h5>Login
            <form class="form-horizontal" method="post" action="@{toMaster loginAuthRoute}">
                <div class="form-group">
                    <label for="login-username" class="col-sm-4 control-label">Login or email
                    <div class="col-sm-8">
                        <input type="text" class="form-control" id="login-username" name="username">
                <div class="form-group">
                    <label for="login-password" class="col-sm-4 control-label">Password
                    <div class="col-sm-8">
                        <input type="password" class="form-control" id="login-password" name="password">
                <div class="form-group">
                    <div class="col-sm-12" style="text-align: right">
                        <button type="submit" class="btn btn-default">Login
|]


