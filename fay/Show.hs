{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax  #-}

module Show where


import JQuery
import Prelude
import qualified EpisodeStatus as ES


main :: Fay ()
main = ready $ do
    ES.setup

