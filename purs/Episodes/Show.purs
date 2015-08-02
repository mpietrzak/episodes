
module Episodes.Show (
    main
) where


import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import DOM
import qualified Control.Monad.Eff.JQuery as J
import qualified Episodes.EpisodeStatus as ES
import qualified Episodes.EpisodeLinks as EL


main :: forall eff. Eff (dom :: DOM, console :: CONSOLE | eff) Unit
main = do
    J.ready $ do
        ES.main
        EL.main
    return unit