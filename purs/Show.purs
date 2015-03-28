

module ShowDetails
where


import qualified Debug.Trace as DT
import qualified EpisodeLinks as EL
import qualified EpisodeStatus as ES



main = do
    ES.main
    EL.main
    DT.trace "ShowDetails: main: done"

