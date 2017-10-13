
module Episodes.ShowSubscriptions (
    bindButtonActions
) where


import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE(), log)
import Control.Monad.Eff.JQuery as J
import Data.Maybe (Maybe(Just, Nothing))
import Data.String as S
import DOM (DOM)
import Global (readInt)
import Prelude

import Episodes.Common as C


-- | Extract ShowId (Number) from subscription link (JQuery).
getSubscriptionLinkShowId :: forall e. J.JQuery -> Eff (console :: CONSOLE, dom :: DOM | e) (Maybe Number)
getSubscriptionLinkShowId link = do
    x0 <- J.closest ".episodes-show" link
    log $ "getSubscriptionLinkShowId: closest .episodes-show: " <> show (C.size x0)
    x1 <- J.find ".show-id" x0
    log $ "getSubscriptionLinkShowId: .show-id: " <> show (C.size x1)
    x2 <- C.getValueText x1
    log $ "getSubscriptionLinkShowId: x2: " <> x2
    let x3 = readInt 10 x2
    pure $ Just x3


onSetSubscriptionStatusDone :: forall e. C.JQueryXmlHttpData -> String -> C.JQueryXmlHttpRequest -> Eff (console :: CONSOLE | e) Unit
onSetSubscriptionStatusDone _ s _ = do
    log $ "onSetSubscriptionStatusDone: " <> s
    pure unit


onSetSubscriptionStatusFail :: forall e. C.JQueryXmlHttpRequest -> String -> String -> Eff (console :: CONSOLE | e) Unit
onSetSubscriptionStatusFail _ s r = do
    log $ "onSetSubscriptionStatusFail: " <> s <> ", " <> r
    pure unit


onSubscriptionButtonClick :: forall e. Boolean -> J.JQueryEvent -> J.JQuery -> Eff (console :: CONSOLE, dom :: DOM | e) Unit
onSubscriptionButtonClick status e target = do
    J.preventDefault e
    ma <- C.getAuthId
    case ma of
        Nothing -> do
            log "not logged in"
            C.redirect "/auth/login"
            pure unit
        Just _ -> do
            log $ "auth id: " <> show ma
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
                    let showsClass = ".show-" <> (S.takeWhile (\c -> c /= '.') (show showId)) <> ".episodes-show"
                    showElements <- J.select showsClass
                    log $ "found " <> show (C.size showElements) <> " show elements of class " <> showsClass
                    case status of
                        true -> do
                            J.removeClass "not-subscribed" showElements
                            J.addClass "subscribed" showElements
                        false -> do
                            J.removeClass "subscribed" showElements
                            J.addClass "not-subscribed" showElements
                    pure unit


bindButtonActions :: forall e. Eff (console :: CONSOLE, dom :: DOM | e) Unit
bindButtonActions = do
    subButtons <- J.select ".subscribe-show"
    unsubButtons <- J.select ".unsubscribe-show"
    case C.size subButtons of
        0 -> log "no sub buttons found"
        _ -> pure unit
    _ <- C.on "click" (onSubscriptionButtonClick true) subButtons
    _ <- C.on "click" (onSubscriptionButtonClick false) unsubButtons
    pure unit

