

module Episodes.Trace (traceValue)
where


import Prelude
import qualified Debug.Trace as Trace


traceValue :: Prelude.Show x => String -> x -> x
traceValue msg x =
    Trace.trace (msg ++ ": " ++ show x) x
