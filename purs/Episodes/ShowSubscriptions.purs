
module Episodes.ShowSubscriptions (
    bindButtonActions
) where


import Prelude (Unit, unit, return, bind, show, (++), ($), (/=))
import Global (readInt)
import Control.Monad.Eff
import Control.Monad.Eff.Console (CONSOLE(), log)
import Data.Maybe (Maybe(Just, Nothing))
import DOM (DOM)
import Control.Monad.Eff.JQuery as J
import Data.String as S

import Episodes.Common as C


-- | Extract ShowId (Number) from subscription link (JQuery).
getSubscriptionLinkShowId :: forall e. J.JQuery -> Eff (console :: CONSOLE, dom :: DOM | e) (Maybe Number)
getSubscriptionLinkShowId link = do
    x0 <- J.closest ".episodes-show" link
    log $ "getSubscriptionLinkShowId: closest .episodes-show: " ++ (show (C.size x0))
    x1 <- J.find ".show-id" x0
    log $ "getSubscriptionLinkShowId: .show-id: " ++ (show (C.size x1))
    x2 <- C.getValueText x1
    log $ "getSubscriptionLinkShowId: x2: " ++ x2
    let x3 = readInt 10 x2
    return $ Just x3


onSetSubscriptionStatusDone :: forall e. C.JQueryXmlHttpData -> String -> C.JQueryXmlHttpRequest -> Eff (console :: CONSOLE | e) Unit
onSetSubscriptionStatusDone _ s _ = do
    log $ "onSetSubscriptionStatusDone: " ++ s
    return unit


onSetSubscriptionStatusFail :: forall e. C.JQueryXmlHttpRequest -> String -> String -> Eff (console :: CONSOLE | e) Unit
onSetSubscriptionStatusFail _ s r = do
    log $ "onSetSubscriptionStatusFail: " ++ s ++ ", " ++ r
    return unit


onSubscriptionButtonClick :: forall e. Boolean -> J.JQueryEvent -> J.JQuery -> Eff (console :: CONSOLE, dom :: DOM | e) Unit
onSubscriptionButtonClick status e target = do
    J.preventDefault e
    ma <- C.getAuthId
    case ma of
        Nothing -> do
            log "not logged in"
            C.redirect "/auth/login"
            return unit
        Just _ -> do
            log $ "auth id: " ++ show ma
            mShowId <- getSubscriptionLinkShowId target
            case mShowId of
                Nothing -> log $ "can't find show id"
                Just showId -> do
                    let req = { status: status, showId: showId }
                    reqJson <- C.jsonStringify req
                    let settings = { "data": reqJson
                            , "url": "/api/set-show-subscription-status"
                            , "method": "POST"
                            , "dataType": "json" }
                    r0 <- C.ajax settings
                    r1 <- C.jqXhrDone r0 onSetSubscriptionStatusDone
                    r2 <- C.jqXhrFail r1 onSetSubscriptionStatusFail
                    -- three of those
                    let showsClass = ".show-" ++ (S.takeWhile (_ /= '.') (show showId)) ++ ".episodes-show"
                    showElements <- J.select showsClass
                    log $ "found " ++ show (C.size showElements) ++ " show elements of class " ++ showsClass
                    case status of
                        true -> do
                            J.removeClass "not-subscribed" showElements
                            J.addClass "subscribed" showElements
                        false -> do
                            J.removeClass "subscribed" showElements
                            J.addClass "not-subscribed" showElements
                    return unit


bindButtonActions :: forall e. Eff (console :: CONSOLE, dom :: DOM | e) Unit
bindButtonActions = do
    subButtons <- J.select ".subscribe-show"
    unsubButtons <- J.select ".unsubscribe-show"
    case C.size subButtons of
        0 -> log "no sub buttons found"
        _ -> return unit
    C.on "click" (onSubscriptionButtonClick true) subButtons
    C.on "click" (onSubscriptionButtonClick false) unsubButtons
    return unit

