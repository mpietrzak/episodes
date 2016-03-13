
module Episodes.ShowSearch (main) where

import Prelude (Unit, bind, map, return, unit, ($), (++), (-), (>), (>>=), (/=), (&&))
import Prelude as P
import Data.Array as A
import Data.Date (Date)
import Data.Date as DD
import Data.Either (Either(Left, Right))
import Data.Foldable (for_)
import Data.Foreign (Foreign, isUndefined, parseJSON, readArray, readBoolean)
import Data.Foreign.Class (readProp)
import Data.Int (toNumber)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Time as DT
import Control.Monad (when)
import Control.Monad.Aff as AFF
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE(), log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.JQuery as J
import Control.Monad.Eff.Ref as R
import DOM (DOM())
import Network.HTTP.Affjax as AFX
import Text.Format as TF

import Episodes.Common as C
import Episodes.ShowSubscriptions as ESS


type SearchState = { query :: String
                   , time :: Date
                   , waiting :: Boolean }

type Show = { showTitle :: String
            , showId :: String
            , showSubscribed :: Boolean }

type SearchResult = { searchResultStatus :: String
                    , searchResultShows :: Array Show }


msOff :: Int
msOff = 200


parseShows :: Array Foreign -> Either String (Array Show)
parseShows foreigns = case firstLeft eitherShows of
                           Just _left -> Left _left
                           _ -> Right $ rights eitherShows
    where
        eitherShows = map parseShow foreigns
        firstLeft eithers = A.head (lefts eithers)
        lefts eithers = A.mapMaybe (\_e -> case _e of
                                                Left _l -> Just _l
                                                _ -> Nothing)
                                 eithers
        rights eithers = A.mapMaybe (\_e -> case _e of
                                                 Right _r -> Just _r
                                                 _ -> Nothing)
                                    eithers
        readBooleanOrUndefined :: Foreign -> Boolean
        readBooleanOrUndefined f = if isUndefined f
            then false
            else case readBoolean f of
                Left _err -> false
                Right _b -> _b
        parseShow f = case readProp "id" f of
            Left _err -> Left $ P.show _err
            Right _id -> case readProp "title" f of
                Left _err -> Left $ P.show _err
                Right _title -> case readProp "subscribed" f of
                    Left _err -> Left $ P.show _err
                    Right _subscribed -> Right { showId: _id
                                               , showTitle: _title
                                               , showSubscribed: (readBooleanOrUndefined _subscribed) }


parseSearchResult :: String -> Either String SearchResult
parseSearchResult s =
   case parseJSON s of
        Left _e -> Left $ P.show _e
        Right _p -> case readProp "status" _p of
            Left _e -> Left $ P.show _e
            Right _s -> case _s of
                "ok" -> case readProp "shows" _p of
                    Left _e -> Left $ P.show _e
                    Right _shows -> case readArray _shows of
                        Left _e -> Left $ P.show _e
                        Right _showsArray -> case parseShows _showsArray of
                            Left _e -> Left $ P.show _e
                            Right _parsedShows -> Right { searchResultStatus: _s
                                                        , searchResultShows: _parsedShows }
                _ -> Right { searchResultStatus: _s, searchResultShows: [] }


showShows :: forall e. Array Show -> Eff (console :: CONSOLE, dom :: DOM | e) Unit
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
        return unit
    where
        makeRow :: forall e'. Show -> Eff (dom :: DOM | e') J.JQuery
        makeRow _show = do
                row <- J.create "<tr>"
                td <- J.create "<td>"
                J.addClass "episodes-show" row
                J.addClass ("show-" ++ _showIdString) row
                J.addClass _statusClass row
                showIdInput <- J.create "<input>"
                J.setAttr "type" "hidden" showIdInput
                J.setAttr "class" "show-id" showIdInput
                J.setAttr "value" _showIdString showIdInput
                J.append showIdInput td
                btn1 <- _makeSubscriptionButton true
                btn2 <- _makeSubscriptionButton false
                J.append btn1 td
                J.append btn2 td
                link <- J.create "<a>"
                J.appendText _show.showTitle link
                J.setAttr "href" ("/show/" ++ _showIdString) link
                J.append link td
                J.append td row
                return row
            where
                _showIdString = TF.format (TF.precision 0) _show.showId
                _statusClass = case _show.showSubscribed of
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
                        return a
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
            err :: EXCEPTION,
            now :: DD.Now,
            ref :: R.REF | e) Unit
search ref query = do
    _ <- do
        now <- DD.now
        R.writeRef ref (Just { time: now, query: query, waiting: true })
    AFF.launchAff $ do
        res <- AFX.get ("/api/shows?q=" ++ query)
        liftEff $ do
            now <- DD.now
            R.modifyRef ref (\ms -> case ms of
                    Just s -> Just $ s { time = now, waiting = false }
                    Nothing -> Nothing)
        case parseSearchResult res.response of
             Left _err -> liftEff $ log $ "ShowSearch.search.aff: err: " ++ _err
             Right _result -> case _result.searchResultStatus of
                "ok" -> liftEff $ do
                    -- log $ "ShowSearch.search.aff: got " ++ (P.show (A.length _result.searchResultShows)) ++ " shows"
                    showShows _result.searchResultShows
                _ -> liftEff $ do
                    log $ "ShowSearch.search.aff: got response with status " ++ _result.searchResultStatus


maybeSearch :: forall e.
                R.Ref (Maybe SearchState)
             -> Eff (
                    ajax :: AFX.AJAX,
                    console :: CONSOLE,
                    dom :: DOM,
                    err :: EXCEPTION,
                    now :: DD.Now,
                    ref :: R.REF | e) Unit
maybeSearch ref = do
    -- Something relevant to search happened, and we should maybe to search AJAX request or schedule something in near future
    now <- DD.now
    -- how much time passed since we sent last request?
    query <- J.select "#query" >>= C.getValueText
    rv <- R.readRef ref
    let shouldSearch = case rv of
            Nothing -> true
            Just _searchState -> _checkShouldSearch now query _searchState
    when shouldSearch $ search ref query
    return unit
    where
        _checkShouldSearch _now _query _searchState = _itIsTime && _queryChanged
            where
                _ms = (DD.toEpochMilliseconds _now) - (DD.toEpochMilliseconds _searchState.time)
                _itIsTime = _ms > (DT.Milliseconds (toNumber msOff))
                _queryChanged = _query /= _searchState.query


onSearchKey :: forall e.
               R.Ref (Maybe SearchState)
            -> J.JQueryEvent -> J.JQuery
            -> Eff (
                    ajax :: AFX.AJAX,
                    console :: CONSOLE,
                    dom :: DOM,
                    err :: EXCEPTION,
                    now :: DD.Now,
                    ref :: R.REF | e
                ) Unit
onSearchKey ref event source = do
    AFF.runAff
        (\_ -> return unit)
        (\_ -> return unit)
        (AFF.later' 0 (liftEff (maybeSearch ref)))
    AFF.runAff
        (\_ -> return unit)
        (\_ -> return unit)
        (AFF.later' msOff (liftEff (maybeSearch ref)))


bindEvents :: forall e.
              R.Ref (Maybe SearchState)
           -> Eff (ajax :: AFX.AJAX, dom :: DOM, console :: CONSOLE, err :: EXCEPTION, now :: DD.Now, ref :: R.REF | e) Unit
bindEvents ref = do
    log "ShowSearch.bindEvents"
    queryBox <- J.select "#query"
    J.on "keyup" (onSearchKey ref) queryBox
    return unit


main :: forall eff. Eff (ajax :: AFX.AJAX, dom :: DOM, console :: CONSOLE, err :: EXCEPTION, now :: DD.Now, ref :: R.REF | eff) Unit
main = do
    ref <- R.newRef Nothing
    J.ready $ do
        bindEvents ref
        maybeSearch ref
    return unit


