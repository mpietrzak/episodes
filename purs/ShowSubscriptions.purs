
module ShowSubscriptions where


import Prelude
import Global
import Control.Monad
import Control.Monad.Eff
import Data.Maybe
import DOM
import qualified Control.Monad.JQuery as J
import qualified Debug.Trace as DT

import Common


-- | Extract ShowId (Number) from subscription link (JQuery).
getSubscriptionLinkShowId :: forall e. J.JQuery -> Eff (dom :: DOM | e) Number
getSubscriptionLinkShowId link = do
    x0 <- closest ".episodes-show" link
    x1 <- find ".show-id" x0
    x2 <- getValueText x1
    let x3 = readInt 10 x2
    return x3


onSetSubscriptionStatusDone :: forall e. JQueryXmlHttpData -> String -> JQueryXmlHttpRequest -> Eff (trace :: DT.Trace | e) {}
onSetSubscriptionStatusDone _ s _ = do
    DT.trace $ "onSetSubscriptionStatusDone: " ++ s
    return {}


onSetSubscriptionStatusFail :: forall e. JQueryXmlHttpRequest -> String -> String -> Eff (trace :: DT.Trace | e) {}
onSetSubscriptionStatusFail _ s r = do
    DT.trace $ "onSetSubscriptionStatusFail: " ++ s ++ ", " ++ r
    return {}


onSubscriptionButtonClick :: forall e. Boolean -> J.JQueryEvent -> J.JQuery -> Eff (trace :: DT.Trace, dom :: DOM | e) Unit
onSubscriptionButtonClick status e target = do
    J.preventDefault e
    ma <- getAuthId
    case ma of
        Nothing -> redirect "/auth/login"
        Just _ -> do
            DT.trace $ "auth id: " ++ show ma
            showId <- getSubscriptionLinkShowId target
            let req = { status: status, showId: showId }
            reqJson <- jsonStringify req
            let settings = { "data": reqJson
                    , "url": "/api/set-show-subscription-status"
                    , "method": "POST"
                    , "dataType": "json" }
            r0 <- ajax settings
            r1 <- jqXhrDone r0 onSetSubscriptionStatusDone
            r <- jqXhrFail r1 onSetSubscriptionStatusFail
            -- three of those
            let showsClass = ".show-" ++ show showId ++ ".episodes-show"
            showElements <- J.select showsClass
            case status of
                true -> do
                    J.removeClass "not-subscribed" showElements
                    J.addClass "subscribed" showElements
                false -> do
                    J.removeClass "subscribed" showElements
                    J.addClass "not-subscribed" showElements
            return unit


bindButtonActions :: forall e. Eff (trace :: DT.Trace, dom :: DOM | e) {}
bindButtonActions = do
    subButtons <- J.select ".subscribe-show"
    unsubButtons <- J.select ".unsubscribe-show"
    on "click" (onSubscriptionButtonClick true) subButtons
    on "click" (onSubscriptionButtonClick false) unsubButtons
    DT.trace "ShowSubscriptions: bindButtonActions: done"
    return {}


main = J.ready bindButtonActions

