
module Calendar
where

import Prelude
import Control.Monad
import Control.Monad.Eff

import qualified Debug.Trace as DT
import qualified EpisodeStatus as ES
import qualified EpisodeLinks as EL


main = do
    ES.main
    EL.main

