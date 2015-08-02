
module Episodes.Calendar where


import Prelude
import Control.Apply ((*>))
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.JQuery (ready)
import DOM (DOM())

import qualified Episodes.EpisodeLinks as EL
import qualified Episodes.EpisodeStatus as ES


main :: forall eff. Eff (dom :: DOM, console :: CONSOLE | eff) Unit
main = ready (EL.main *> ES.main) *> return unit -- ready returns JQuery

