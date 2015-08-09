
module Episodes.SeasonExpandCollapse (
    main
) where


import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE(), log)
import Data.Int (fromString)
import Data.Maybe
import DOM (DOM())
import qualified Control.Monad.Eff.JQuery as J

import qualified Episodes.Common as C


onAjaxSeasonCollapseDone :: forall e. C.JQueryXmlHttpData -> String -> C.JQueryXmlHttpRequest -> Eff (console :: CONSOLE | e) Unit
onAjaxSeasonCollapseDone _ _ _  = do
    log "done"
    return unit


onAjaxSeasonCollapseFail :: forall e. C.JQueryXmlHttpRequest -> String -> String -> Eff (console :: CONSOLE | e) Unit
onAjaxSeasonCollapseFail _ _ _ = do
    log "fail"
    return unit


ajaxSetSeasonCollapse :: forall eff. Boolean -> Int -> Eff (console :: CONSOLE, dom :: DOM | eff) Unit
ajaxSetSeasonCollapse collapse season = do
    log $ "ajaxSetSeasonCollapse: setting collapse to: " ++ show collapse
    req <- C.jsonStringify $ { season: season
                             , collapse: collapse }
    log $ "req: " ++ req
    let settings = { "data": req
                   , "url": "/api/set-season-collapse"
                   , "method": "POST"
                   , "dataType": "json" }
    r0 <- C.ajax settings
    r1 <- C.jqXhrDone r0 onAjaxSeasonCollapseDone
    r2 <- C.jqXhrFail r1 onAjaxSeasonCollapseFail
    return unit


getSeasonRow :: forall eff. J.JQuery -> Eff (dom :: DOM | eff) J.JQuery
getSeasonRow row = do
    isSeason <- C.is "tr.season" row
    case isSeason of
        true -> return row
        false -> do
            allPrevSeasonRows <- C.prevAll "tr.season" row
            C.first allPrevSeasonRows


onSeasonMouseEnter :: forall eff. J.JQueryEvent -> J.JQuery -> Eff (console :: CONSOLE, dom :: DOM | eff) Unit
onSeasonMouseEnter event jquery = do
    seasonRow <- getSeasonRow jquery
    expandCollapseLinks <- J.find "div.collapse-season-hover" seasonRow
    J.css {visibility: "visible"} expandCollapseLinks
    return unit


onSeasonMouseLeave :: forall eff. J.JQueryEvent -> J.JQuery -> Eff (console :: CONSOLE, dom :: DOM | eff) Unit
onSeasonMouseLeave event jquery = do
    seasonRow <- getSeasonRow jquery
    expandCollapseLinks <- J.find "div.collapse-season-hover" seasonRow
    J.css {visibility: "hidden"} expandCollapseLinks
    return unit


onExpandCollapseClick :: forall eff. Boolean -> J.JQueryEvent -> J.JQuery -> Eff (console :: CONSOLE, dom :: DOM | eff) Unit
onExpandCollapseClick _which event link = do
    J.preventDefault event
    seasonRow <- J.closest "tr.season" link
    mSeasonId <- J.find "td.show-season" seasonRow >>= C.getAttr "data-season-id" >>= \s -> return $ fromString s
    case mSeasonId of
        Nothing -> do
            log "no season number"
            return unit
        Just seasonId -> do
            ajaxSetSeasonCollapse (not _which) seasonId
            episodeRows <- C.nextUntil "tr.season" seasonRow
            expandLinkDiv <- J.find "div.expand-season" seasonRow
            collapseLinkDiv <- J.find "div.collapse-season" seasonRow
            case _which of
                true -> do
                    J.css { display: "table-row" } episodeRows
                    J.css { display: "none" } expandLinkDiv
                    J.css { display: "block" } collapseLinkDiv
                false -> do
                    J.css { display: "none" } episodeRows
                    J.css { display: "block" } expandLinkDiv
                    J.css { display: "none" } collapseLinkDiv
            return unit


main :: forall eff. Eff (dom :: DOM, console :: CONSOLE | eff) Unit
main = do
    J.ready $ do
        log "SeasonExpandCollapse.main/ready start"
        trs <- J.select "tr.season, tr.episode"
        log $ "SeasonExpandCollapse: have " ++ show (C.size trs) ++ " trs"
        J.on "mouseenter" onSeasonMouseEnter trs
        J.on "mouseleave" onSeasonMouseLeave trs
        expandLinks <- J.select "tr.season td a.expand-season"
        collapseLinks <- J.select "tr.season td a.collapse-season"
        J.on "click" (onExpandCollapseClick true) expandLinks
        J.on "click" (onExpandCollapseClick false) collapseLinks
        log "SeasonExpandCollapse.main/ready done"
    return unit


