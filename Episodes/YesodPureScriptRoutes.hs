{- # LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Episodes.YesodPureScriptRoutes
where

import Control.Monad (return)
import System.IO (IO)
import Data.Text (Text)
--import Yesod.Core ( HandlerT
--                  , Route
--                  , Texts
--                  , Yesod
--                  , mkYesodSubData
--                  , parseRoutes
--                  , renderRoute )
import           Yesod.Core


-- The PureScript subsite.
data PureScriptSite = PureScriptSite


mkYesodSubData "PureScriptSite" [parseRoutes|
/*Texts PureScriptR GET
|]


class Yesod master => YesodPureScript master where
    name :: HandlerT master IO Text
    name = return "purescript"

