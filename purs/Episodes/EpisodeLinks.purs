
module Episodes.EpisodeLinks where


import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.JQuery as J
import Data.Either (Either(Left, Right))
import Data.Foldable (foldl)
import Data.String (Pattern(Pattern), Replacement(Replacement), replace)
import Data.String.Regex as DSR
import Data.Traversable (for, for_)
import DOM (DOM)
import Global (readInt)
import Prelude

import Episodes.Common as C


type EpisodeInfo = { showTitle :: String
                   , episodeTitle :: String
                   , episodeSeasonNumber :: Number
                   , episodeNumber :: Number
                   , episodeCode :: String
                   , episodeTVRageId :: Number }



-- | Format episode link, for example.
-- Example Input: http://www.google.com/search?q={episode.title}
-- Example Output: http://www.google.com/search?q=The+Drone+Queen
formatEpisodeLink :: EpisodeInfo -> String -> String
formatEpisodeLink _ei _linkTemplate = _fmt _linkTemplate
    where
        -- list of replacing functions
        _f = [replace (Pattern "{{episode.title}}") (Replacement _ei.episodeTitle),
              replace (Pattern "{{episode.code}}") (Replacement _ei.episodeCode),
              replace (Pattern "{{episode.season}}") (Replacement (show _ei.episodeSeasonNumber)),
              replace (Pattern "{{show.title}}") (Replacement _ei.showTitle)]
        -- formatting function as a composition of list of functions
        _fmt = foldl (>>>) id _f


formatEpisodeLinks :: EpisodeInfo -> String -> Array String
formatEpisodeLinks _episodeInfo _linksFormat = map (formatEpisodeLink _episodeInfo) linkPatterns
    where
        linkPatterns = case DSR.regex "[\\r\\n]+" (DSR.parseFlags "") of
            Right _r -> C.split _r _linksFormat
            Left _ -> []


-- | Assumes there's a hidden input.episode-id inside closest .episode.
getParentEpisodeId :: forall eff. J.JQuery -> Eff (dom :: DOM, console :: CONSOLE | eff) Number
getParentEpisodeId el = do
    x0 <- J.closest ".episode" el
    x1 <- J.find ".episode-id" x0
    x2 <- C.getValueText x1
    let x3 = readInt 10 x2
    pure x3


-- | Get user links as a list of String, assumes there's hidden
-- input element with id "user-episode-links" somewhere in the page.
getUserEpisodeLinks :: forall eff. Eff (dom :: DOM, console :: CONSOLE | eff) String
getUserEpisodeLinks = do
    _linksInput <- J.select "#user-episode-links"
    _links <- C.getValueText _linksInput
    pure _links


-- | Mouse over tr: show "more" link.
onEpisodeMouseEnter :: forall e. J.JQueryEvent -> J.JQuery -> Eff (console :: CONSOLE, dom :: DOM | e) Unit
onEpisodeMouseEnter _e _j = do
    _moreLink <- J.find "a.episode-links" _j
    -- C.animate {"opacity": 1} 100 _moreLink
    J.css {visibility: "visible"} _moreLink
    pure unit


onEpisodeMouseLeave :: forall e. J.JQueryEvent -> J.JQuery -> Eff (console :: CONSOLE, dom :: DOM | e) Unit
onEpisodeMouseLeave _e _j = do
    _moreLink <- J.find "a.episode-links" _j
    -- C.animate {"opacity": 0} 100 _moreLink
    J.css {visibility: "hidden"} _moreLink
    pure unit


-- | Get episode data from DOM.
getEpisodeInfo :: forall e. J.JQuery -> Eff (dom :: DOM, console :: CONSOLE | e) EpisodeInfo
getEpisodeInfo _el = do
    episodeTitle <- J.find "input.episode-title" _el >>= C.getValueText
    showTitle <- J.find "input.episode-show-title" _el >>= C.getValueText
    episodeSeasonNumber <- J.find "input.episode-show-title" _el >>= C.getValueText >>= \t -> pure (readInt 10 t)
    episodeNumber <- J.find "input.episode-number" _el >>= C.getValueText >>= \t -> pure (readInt 10 t)
    episodeCode <- J.find "input.episode-code" _el >>= C.getValueText
    episodeTVRageId <- J.find "input.episode-tvrageid" _el >>= C.getValueText >>= \t -> pure (readInt 10 t)
    let ei = { showTitle: showTitle
             , episodeTitle: episodeTitle
             , episodeSeasonNumber: episodeSeasonNumber
             , episodeNumber: episodeNumber
             , episodeCode: episodeCode
             , episodeTVRageId: episodeTVRageId }
    pure ei


createEpisodeLink :: forall e. String -> Eff (dom :: DOM, console :: CONSOLE | e) J.JQuery
createEpisodeLink _href = do
    e <- J.create "<a>"
    J.setText _href e
    J.setAttr "href" _href e
    J.setAttr "target" "_blank" e
    pure e


onEpisodeLinksDivLeave :: forall e. J.JQueryEvent -> J.JQuery -> Eff (dom :: DOM, console :: CONSOLE | e) Unit
onEpisodeLinksDivLeave _e _div = do
    log "leave links div"
    _ <- C.jQueryFadeOut' 100 (J.remove _div) _div
    pure unit


-- | Episode "more" link was clicked, we need to format and show links for this episode.
-- We also bind mouseleave event to hide and destroy links div.
onEpisodeLinkClick :: forall eff. String -> J.JQueryEvent -> J.JQuery -> Eff (dom :: DOM, console :: CONSOLE | eff) Unit
onEpisodeLinkClick _linksTemplate _e _j = do
    log "click"

    -- prepare, get vars
    J.preventDefault _e
    x <- C.getJQueryEventPageX _e
    y <- C.getJQueryEventPageY _e
    _episodeElement <- J.closest ".episode" _j
    _episodeInfo <- getEpisodeInfo _episodeElement

    -- format links
    let _episodeLinks = formatEpisodeLinks _episodeInfo _linksTemplate :: Array String

    -- create inner div, for display
    _div <- J.create "<div>"
    _ul <- J.create "<ul>"
    _links <- for _episodeLinks createEpisodeLink
    log $ "_links formatted: " <> show _episodeLinks
    let _addLink _link = do
            _li <- J.create "<li>"
            J.append _link _li
            J.append _li _ul
    for_ _links _addLink
    J.append _ul _div
    let _infoText = "You can change those links in profile preferences."
    p <- J.create "<p>"
    J.addClass "info" p
    J.setText _infoText p
    J.append p _div

    let _divCss = {
                border: "1px solid black",
                padding: "4pt",
                "background-color": "white"
            }
    J.css _divCss _div
    J.addClass "episode-links" _div

    -- create outer div, for mouse leave event
    _outerDiv <- J.create "<div>"
    J.css {position: "absolute", left: x - 32, top: y - 32, padding: 16, display: "none"} _outerDiv

    -- add to dom, show and bind leave event
    _parent <- J.parent _j
    J.append _div _outerDiv
    J.append _outerDiv _parent
    _ <- C.jQueryFadeIn 50 _outerDiv
    _ <- C.on "mouseleave" onEpisodeLinksDivLeave _outerDiv

    pure unit


bindEvents :: forall eff. String -> Eff (dom :: DOM, console :: CONSOLE | eff) Unit
bindEvents _linksTemplate = do
    _episodes <- J.select "tr.episode"
    _episodeMoreLink <- J.select "tr.episode a.episode-links"
    _ <- C.on "mouseenter" onEpisodeMouseEnter _episodes
    _ <- C.on "mouseleave" onEpisodeMouseLeave _episodes
    _ <- C.on "click" (onEpisodeLinkClick _linksTemplate) _episodeMoreLink
    log "EpisodeLinks: bindEvents: done"
    pure unit


main :: forall eff. Eff (dom :: DOM, console :: CONSOLE | eff) Unit
main = J.ready (getUserEpisodeLinks >>= bindEvents) >>= \_ -> pure unit
