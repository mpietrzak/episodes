
module Episodes.ShowSearch (main) where


import Control.Monad.Aff as AFF
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE(), log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.JQuery as J
import Control.Monad.Eff.Now as EffNow
import Control.Monad.Eff.Ref as R
import Data.Argonaut.Core (Json)
import Data.DateTime.Instant (Instant, unInstant)
import Data.Either (Either(Left, Right))
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Time.Duration (Milliseconds(Milliseconds))
import DOM (DOM())
import Network.HTTP.Affjax as AFX
import Prelude
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))

import Episodes.Common as C
import Episodes.ShowSubscriptions as ESS


type SearchState = { query :: String
                   , time :: Instant
                   , waiting :: Boolean }


data Show = Show { title :: String
                 , id :: String
                 , subscribed :: Boolean }


data SearchResult = SearchResult { status :: String
                                 , shows :: Array Show }



instance decodeJsonSearchResult :: DecodeJson SearchResult where
    decodeJson json = do
        o <- decodeJson json
        status <- o .? "status"
        shows <- o .? "shows"
        pure $ SearchResult {
            status: status,
            shows: shows }


instance decodeJsonShow :: DecodeJson Show where
    decodeJson json = do
        o <- decodeJson json
        title <- o .? "title"
        id <- o .? "id"
        subscribed <- o .? "subscribed"
        pure $ Show {
            id: id,
            title: title,
            subscribed: subscribed }


msOff :: Int
msOff = 200


parseSearchResult :: Json -> Either String SearchResult
parseSearchResult s = do
    case decodeJson s :: Either String SearchResult of
        Left err -> Left err
        Right r -> Right r


showShows :: forall e. Array Show -> Eff ( console :: CONSOLE
                                         , dom :: DOM | e) Unit
showShows shows = do
        resultsBox <- J.select "#shows"
        resultsTable <- J.create "<table>"
        J.addClass "shows" resultsTable
        resultsTbody <- J.create "<tbody>"
        J.clear resultsBox
        for_ shows (\_show -> do
            showRow <- makeRow _show
            J.append showRow resultsTbody)
        J.append resultsTbody resultsTable
        J.append resultsTable resultsBox
        ESS.bindButtonActions
        pure unit
    where
        -- makeRow :: forall e. Show -> Eff (dom :: DOM | e) J.JQuery
        makeRow _showData = do
                case _showData of
                    Show _show -> do
                        row <- J.create "<tr>"
                        td <- J.create "<td>"
                        J.addClass "episodes-show" row
                        J.addClass ("show-" <> _show.id) row
                        J.addClass (_statusClass _show) row
                        showIdInput <- J.create "<input>"
                        J.setAttr "type" "hidden" showIdInput
                        J.setAttr "class" "show-id" showIdInput
                        J.setAttr "value" _show.id showIdInput
                        J.append showIdInput td
                        btn1 <- _makeSubscriptionButton true
                        btn2 <- _makeSubscriptionButton false
                        J.append btn1 td
                        J.append btn2 td
                        link <- J.create "<a>"
                        J.appendText _show.title link
                        J.setAttr "href" ("/show/" <> _show.id) link
                        J.append link td
                        J.append td row
                        pure row
        _statusClass _sh = case _sh.subscribed of
                            true -> "subscribed"
                            false -> "not-subscribed"
        _makeSubscriptionButton which = do
                a <- J.create "<a>"
                J.setAttr "href" "#" a
                J.addClass "btn" a
                J.addClass "btn-default" a
                J.addClass "btn-xs" a
                if which then J.addClass "btn-success" a
                         else J.addClass "btn-danger" a
                J.addClass _linkClass a
                J.appendText _linkText a
                pure a
            where
                _linkClass = case which of
                    true -> "subscribe-show"
                    false -> "unsubscribe-show"
                _linkText = case which of
                    true -> "+"
                    false -> "-"

search :: forall e.
          R.Ref (Maybe SearchState)
       -> String
       -> Eff (
            ajax :: AFX.AJAX,
            console :: CONSOLE,
            dom :: DOM,
            now :: EffNow.NOW,
            ref :: R.REF | e) Unit
search ref query = do
    _ <- do
        now <- EffNow.now
        R.writeRef ref (Just { time: now, query: query, waiting: true })
    _ <- AFF.runAff (\_ -> pure unit) (\_ -> pure unit) $ do
        res <- AFX.get ("/api/shows?q=" <> query)
        liftEff $ do
            now <- EffNow.now
            R.modifyRef ref (\ms -> case ms of
                    Just s -> Just $ s { time = now, waiting = false }
                    Nothing -> Nothing)
        case parseSearchResult res.response of
             Left _err -> liftEff $ log $ "ShowSearch.search.aff: err: " <> _err
             Right (SearchResult _result) -> case _result.status of
                "ok" -> liftEff $ do
                    showShows _result.shows
                _ -> liftEff $ do
                    log $ "ShowSearch.search.aff: got response with status " <> _result.status
    pure unit


maybeSearch :: forall e.
                R.Ref (Maybe SearchState)
             -> Eff (
                    ajax :: AFX.AJAX,
                    console :: CONSOLE,
                    dom :: DOM,
                    now :: EffNow.NOW,
                    ref :: R.REF | e) Unit
maybeSearch ref = do
    -- Something relevant to search happened, and we should maybe to search AJAX request or schedule something in near future
    now <- EffNow.now
    -- how much time passed since we sent last request?
    query <- J.select "#query" >>= C.getValueText
    rv <- R.readRef ref
    let shouldSearch = case rv of
            Nothing -> true
            Just _searchState -> _checkShouldSearch now query _searchState
    when shouldSearch $ search ref query
    pure unit
    where
        _checkShouldSearch _now _query _searchState = _itIsTime && _queryChanged
            where
                _ms = (unInstant _now) - (unInstant _searchState.time)
                _itIsTime = _ms > (Milliseconds (toNumber msOff))
                _queryChanged = _query /= _searchState.query


onSearchKey :: forall e.
               R.Ref (Maybe SearchState)
            -> J.JQueryEvent -> J.JQuery
            -> Eff (
                    ajax :: AFX.AJAX,
                    console :: CONSOLE,
                    dom :: DOM,
                    now :: EffNow.NOW,
                    ref :: R.REF | e
                ) Unit
onSearchKey ref event source = do
    runAff_ $ maybeSearchAfterDelay 0.0
    runAff_ $ maybeSearchAfterDelay (toNumber msOff)
    where
        runAff_ x = do
            _ <- AFF.runAff (\_ -> pure unit) (\_ -> pure unit) x
            pure unit
        maybeSearchAfterDelay ms = do
            AFF.delay (Milliseconds ms)
            liftEff (maybeSearch ref)



bindEvents :: forall e.
              R.Ref (Maybe SearchState)
           -> Eff ( ajax :: AFX.AJAX
                  , dom :: DOM
                  , console :: CONSOLE
                  , err :: EXCEPTION
                  , now :: EffNow.NOW
                  , ref :: R.REF | e) Unit
bindEvents ref = do
    log "ShowSearch.bindEvents"
    queryBox <- J.select "#query"
    J.on "keyup" (onSearchKey ref) queryBox
    pure unit


main :: forall eff. Eff ( ajax :: AFX.AJAX
                        , dom :: DOM
                        , console :: CONSOLE
                        , err :: EXCEPTION
                        , now :: EffNow.NOW
                        , ref :: R.REF | eff) Unit
main = do
    ref <- R.newRef Nothing
    J.ready $ do
        bindEvents ref
        queryBox <- J.select "#query"
        when (C.size queryBox > 0) $ maybeSearch ref
    pure unit


