{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
module SharedTypes where


import Prelude
import Data.Data
import Language.Fay.Yesod


data SetEpisodeStatusResult = SetEpisodeStatusResult { setEpisodeStatusResultErrorCode :: Int
                                                     , setEpisodeStatusResultErrorDesc :: Maybe Text
                                                     , setEpisodeStatusResultEpisodeStatus :: Text }
    deriving (Read, Typeable,  Data, Show)


data Command = SetEpisodeStatus Int Bool (Returns SetEpisodeStatusResult)
    deriving (Read, Typeable, Data)

