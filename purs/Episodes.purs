
module Episodes (main) where


import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import DOM

import qualified Episodes.EpisodeLinks as EL
import qualified Episodes.EpisodeStatus as ES
import qualified Episodes.ShowSubscriptions as ESS
import qualified Episodes.SeasonExpandCollapse as ESEC


main :: forall eff. Eff (dom :: DOM, console :: CONSOLE | eff) Unit
main = do
    EL.main
    ES.main
    ESS.main
    ESEC.main
