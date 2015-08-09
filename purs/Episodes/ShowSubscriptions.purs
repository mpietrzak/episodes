
module Episodes.ShowSubscriptions (
    main
) where


import Prelude
import Global
import Control.Apply ((*>))
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Console (CONSOLE(), log)
import Data.Maybe
import DOM
import qualified Control.Monad.Eff.JQuery as J
import qualified Data.String as S

import Episodes.Common


-- | Extract ShowId (Number) from subscription link (JQuery).
getSubscriptionLinkShowId :: forall e. J.JQuery -> Eff (console :: CONSOLE, dom :: DOM | e) (Maybe Number)
getSubscriptionLinkShowId link = do
    x0 <- J.closest ".episodes-show" link
    x1 <- J.find ".show-id" x0
    x2 <- getValueText x1
    let x3 = readInt 10 x2
    return $ Just x3


onSetSubscriptionStatusDone :: forall e. JQueryXmlHttpData -> String -> JQueryXmlHttpRequest -> Eff (console :: CONSOLE | e) Unit
onSetSubscriptionStatusDone _ s _ = do
    log $ "onSetSubscriptionStatusDone: " ++ s
    return unit


onSetSubscriptionStatusFail :: forall e. JQueryXmlHttpRequest -> String -> String -> Eff (console :: CONSOLE | e) Unit
onSetSubscriptionStatusFail _ s r = do
    log $ "onSetSubscriptionStatusFail: " ++ s ++ ", " ++ r
    return unit


onSubscriptionButtonClick :: forall e. Boolean -> J.JQueryEvent -> J.JQuery -> Eff (console :: CONSOLE, dom :: DOM | e) Unit
onSubscriptionButtonClick status e target = do
    J.preventDefault e
    ma <- getAuthId
    case ma of
        Nothing -> do
            log "not logged in"
            redirect "/auth/login"
            return unit
        Just _ -> do
            log $ "auth id: " ++ show ma
            mShowId <- getSubscriptionLinkShowId target
            case mShowId of
                Nothing -> log $ "can't find show id"
                Just showId -> do
                    let req = { status: status, showId: showId }
                    reqJson <- jsonStringify req
                    let settings = { "data": reqJson
                            , "url": "/api/set-show-subscription-status"
                            , "method": "POST"
                            , "dataType": "json" }
                    r0 <- ajax settings
                    r1 <- jqXhrDone r0 onSetSubscriptionStatusDone
                    r2 <- jqXhrFail r1 onSetSubscriptionStatusFail
                    -- three of those
                    let showsClass = ".show-" ++ (S.takeWhile (/= '.') (show showId)) ++ ".episodes-show"
                    showElements <- J.select showsClass
                    log $ "found " ++ show (size showElements) ++ " show elements of class " ++ showsClass
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
    case size subButtons of
        0 -> do
            log "no sub buttons found"
        c -> do
            log $ "found " ++ show c ++ " sub buttons"
    on "click" (onSubscriptionButtonClick true) subButtons
    on "click" (onSubscriptionButtonClick false) unsubButtons
    log "ShowSubscriptions: bindButtonActions: done"
    return unit


main :: forall eff. Eff (console :: CONSOLE, dom :: DOM | eff) Unit
main = do
    J.ready $ do
        log "ShowSubscriptions: main"
        bindButtonActions
        log "ShowSubscriptions: done"
    return unit

