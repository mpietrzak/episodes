
module Episodes (main) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Ref as R
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)

import Episodes.EpisodeLinks as EL
import Episodes.EpisodeStatus as ES
import Episodes.ShowSearch as SS
import Episodes.SeasonExpandCollapse as ESEC


main :: forall eff. Eff (
            ajax :: AJAX,
            dom :: DOM,
            console :: CONSOLE,
            err :: EXCEPTION,
            now :: NOW,
            ref :: R.REF | eff) Unit
main = do
    EL.main
    ES.main
    ESEC.main
    SS.main

