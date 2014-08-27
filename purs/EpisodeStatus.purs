
module EpisodeStatus
where


import Control.Monad
import Control.Monad.Eff
import Data.Foreign
import Data.Maybe
import Prelude
import Global
import qualified Control.Monad.JQuery as J
import qualified Debug.Trace as DT


import Common


-- Assumes there's a hidden input.episode-id inside closest .episode.
getCheckboxEpisodeId checkbox = do
    x0 <- closest ".episode" checkbox
    x1 <- find ".episode-id" x0
    x2 <- getValueText x1
    let x3 = readInt 10 x2
    return x3


getCheckboxStatus checkbox = is ":checked" checkbox


onSetEpisodeStatusDone :: forall e. JQueryXmlHttpData -> String -> JQueryXmlHttpRequest -> Eff (trace :: DT.Trace | e) {}
onSetEpisodeStatusDone _ s _ = do
    DT.trace $ "done: " ++ s
    return {}


onSetEpisodeStatusFail :: forall e. JQueryXmlHttpRequest -> String -> String -> Eff (trace :: DT.Trace | e) {}
onSetEpisodeStatusFail _ s r = do
    DT.trace $ "fail: " ++ s ++ ", " ++ r
    return {}


onEpisodeStatusCheckboxClick :: forall e. J.JQueryEvent -> J.JQuery -> Eff (trace :: DT.Trace, dom :: J.DOM | e) {}
onEpisodeStatusCheckboxClick event target = do
    episodeId <- getCheckboxEpisodeId target
    status <- getCheckboxStatus target
    let req = { episodeStatus: status, episodeId: episodeId }
    reqJson <- jsonStringify req
    -- let ajaxSettings = AjaxSettings { ajaxData: Just reqJson }
    let settings = { "data": reqJson
            , "url": "/api/set-episode-status"
            , "method": "POST"
            , "dataType": "json" }
    r0 <- ajax settings
    r1 <- jqXhrDone r0 onSetEpisodeStatusDone
    r <- jqXhrFail r1 onSetEpisodeStatusFail
    return {}


main = do
    checkboxes <- J.select ".episode-status.checkbox"
    J.on "change" onEpisodeStatusCheckboxClick checkboxes

