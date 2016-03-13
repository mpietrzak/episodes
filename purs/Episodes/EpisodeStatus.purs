
module Episodes.EpisodeStatus (
    main
) where


import Prelude (Unit, unit, return, bind, ($), (++))
import Control.Monad.Eff
import Control.Monad.Eff.Console (CONSOLE(), log)
import Data.Maybe (Maybe(Just, Nothing))
import DOM (DOM())
import Control.Monad.Eff.JQuery as J

import Episodes.Common as C


-- Assumes there's a hidden input.episode-id inside closest .episode.
getCheckboxEpisodeId :: forall e. J.JQuery -> Eff (dom :: DOM | e) (Maybe String)
getCheckboxEpisodeId checkbox = do
    x0 <- J.closest ".episode" checkbox
    x1 <- J.find ".episode-id" x0
    x2 <- C.getValueText x1
    return $ Just x2


getCheckboxStatus :: forall e. J.JQuery -> Eff (dom :: DOM | e) Boolean
getCheckboxStatus checkbox = C.is ":checked" checkbox


onSetEpisodeStatusDone :: forall e. C.JQueryXmlHttpData -> String -> C.JQueryXmlHttpRequest -> Eff (console :: CONSOLE | e) Unit
onSetEpisodeStatusDone _ s _ = do
    log $ "onSetEpisodeStatusDone: " ++ s
    return unit


onSetEpisodeStatusFail :: forall e. C.JQueryXmlHttpRequest -> String -> String -> Eff (console :: CONSOLE | e) Unit
onSetEpisodeStatusFail _ s r = do
    log $ "fail: " ++ s ++ ", " ++ r
    return unit


onEpisodeStatusCheckboxClick :: forall e. J.JQueryEvent -> J.JQuery -> Eff (console :: CONSOLE, dom :: DOM | e) Unit
onEpisodeStatusCheckboxClick event target = do
    authId <- C.getAuthId
    case authId of
        Nothing -> C.redirect "/auth/login"
        Just _ -> do
            mEpisodeId <- getCheckboxEpisodeId target
            case mEpisodeId of
                Nothing -> do
                    -- TOOD: handle error
                    log "invalid checkbox"
                    return unit
                Just episodeId -> do
                    status <- getCheckboxStatus target
                    let req = { episodeStatus: status, episodeId: episodeId }
                    reqJson <- C.jsonStringify req
                    let settings = { "data": reqJson
                            , "url": "/api/set-episode-status"
                            , "method": "POST"
                            , "dataType": "json" }
                    r0 <- C.ajax settings
                    r1 <- C.jqXhrDone r0 onSetEpisodeStatusDone
                    r <- C.jqXhrFail r1 onSetEpisodeStatusFail
                    return unit


main :: forall eff. Eff (dom :: DOM, console :: CONSOLE | eff) Unit
main = do
    J.ready $ do
        checkboxes <- J.select ".episode-status.checkbox"
        C.on "change" onEpisodeStatusCheckboxClick checkboxes
        log "EpisodeStatus: main: done"
    return unit


