{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax  #-}

module Calendar where


import JQuery
import Prelude
import qualified EpisodeStatus as ES


main :: Fay ()
main = ready $ do
    ES.setup

