{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
module SharedTypes where


import Prelude
import Data.Data
import Language.Fay.Yesod

data Command = SubscribeShow Integer (Returns ())
    deriving (Read, Typeable, Data)

