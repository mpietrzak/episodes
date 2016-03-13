
module Episodes (main) where

import Prelude (Unit, bind)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref as R
import Data.Date as DD
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
            now :: DD.Now,
            ref :: R.REF | eff) Unit
main = do
    EL.main
    ES.main
    ESEC.main
    SS.main

