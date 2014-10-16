
module EpisodeLinks where


import Prelude
import Control.Monad
import Control.Monad.Eff
import Data.Array (map)
import Data.Foldable (foldl)
import Data.String
import Data.Traversable (for, sequence)
import Global
import qualified Control.Monad.JQuery as J
import qualified Debug.Trace as DT

import qualified Common as C


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
        _f = [replace "{{episode.title}}" _ei.episodeTitle,
              replace "{{episode.code}}" _ei.episodeCode,
              replace "{{episode.season}}" (show _ei.episodeSeasonNumber),
              replace "{{show.title}}" _ei.showTitle]
        -- formatting function as a composition of list of functions
        _fmt = foldl (>>>) id _f


formatEpisodeLinks :: EpisodeInfo -> String -> [String]
formatEpisodeLinks _episodeInfo _linksFormat = map (formatEpisodeLink _episodeInfo) linkPatterns
    where
        linkPatterns = split "\n" _linksFormat


-- | Assumes there's a hidden input.episode-id inside closest .episode.
getParentEpisodeId :: forall eff. J.JQuery -> Eff (dom :: J.DOM, trace :: DT.Trace | eff) Number
getParentEpisodeId el = do
    x0 <- C.closest ".episode" el
    x1 <- C.find ".episode-id" x0
    x2 <- C.getValueText x1
    let x3 = readInt 10 x2
    return x3


-- | Get user links as a list of String, assumes there's hidden
-- input element with id "user-episode-links" somewhere in the page.
getUserEpisodeLinks :: forall eff. Eff (dom :: J.DOM, trace :: DT.Trace | eff) String
getUserEpisodeLinks = do
    _linksInput <- J.select "#user-episode-links"
    _links <- C.getValueText _linksInput
    DT.trace $ "_links: " ++ _links
    return _links


-- | Mouse over tr: show "more" link.
onEpisodeMouseEnter :: forall e. J.JQueryEvent -> J.JQuery -> Eff (trace :: DT.Trace, dom :: J.DOM | e) Unit
onEpisodeMouseEnter _e _j = do
    _moreLink <- C.find "a.episode-links" _j
    C.animate {"opacity": 1} 100 _moreLink
    return unit


onEpisodeMouseLeave :: forall e. J.JQueryEvent -> J.JQuery -> Eff (trace :: DT.Trace, dom :: J.DOM | e) Unit
onEpisodeMouseLeave _e _j = do
    _moreLink <- C.find "a.episode-links" _j
    C.animate {"opacity": 0} 100 _moreLink
    return unit


-- | Get episode data from DOM.
getEpisodeInfo :: forall e. J.JQuery -> Eff (dom :: J.DOM, trace :: DT.Trace | e) EpisodeInfo
getEpisodeInfo _el = do
    episodeTitle <- C.find "input.episode-title" _el >>= C.getValueText
    showTitle <- C.find "input.episode-show-title" _el >>= C.getValueText
    episodeSeasonNumber <- C.find "input.episode-show-title" _el >>= C.getValueText >>= \t -> return (readInt 10 t)
    episodeNumber <- C.find "input.episode-number" _el >>= C.getValueText >>= \t -> return (readInt 10 t)
    episodeCode <- C.find "input.episode-code" _el >>= C.getValueText
    episodeTVRageId <- C.find "input.episode-tvrageid" _el >>= C.getValueText >>= \t -> return (readInt 10 t)
    let ei = { showTitle: showTitle
             , episodeTitle: episodeTitle
             , episodeSeasonNumber: episodeSeasonNumber
             , episodeNumber: episodeNumber
             , episodeCode: episodeCode
             , episodeTVRageId: episodeTVRageId }
    return ei


createEpisodeLink :: forall e. String -> Eff (dom :: J.DOM, trace :: DT.Trace | e) J.JQuery
createEpisodeLink _href = J.create "<a>"
    >>= J.setText _href
    >>= J.setAttr "href" _href
    >>= J.setAttr "target" "_new"


onEpisodeLinksDivLeave :: forall e. J.JQueryEvent -> J.JQuery -> Eff (dom :: J.DOM | e) Unit
onEpisodeLinksDivLeave _e _div = do
    C.jQueryFadeOut' 100 (J.remove _div) _div
    return unit


-- | Episode "more" link was clicked, we need to format and show links for this episode.
-- We also bind mouseleave event to hide and destroy links div.
onEpisodeLinkClick :: forall e. String -> J.JQueryEvent -> J.JQuery -> Eff (trace :: DT.Trace, dom :: J.DOM | e) Unit
onEpisodeLinkClick _linksTemplate _e _j = do
    DT.trace "click"
    J.preventDefault _e
    _episodeElement <- C.closest ".episode" _j
    _episodeInfo <- getEpisodeInfo _episodeElement

    let _episodeLinks = formatEpisodeLinks _episodeInfo _linksTemplate :: [String]
    _div <- J.create "<div>"
    _ul <- J.create "<ul>"
    _links <- for _episodeLinks createEpisodeLink
    let _addLink _link = do
            _li <- J.create "<li>"
            J.append _link _li
            J.append _li _ul
    for _links _addLink
    J.append _ul _div

    x <- C.getJQueryEventPageX _e
    y <- C.getJQueryEventPageY _e
    let props = {
                position: "absolute",
                left: x - 10,
                top: y - 10,
                border: "1px solid black",
                padding: "1em",
                "background-color": "white",
                display: "none"
            }
    J.css props _div
    J.on "mouseleave" onEpisodeLinksDivLeave _div
    J.addClass "episode-links" _div
    J.append _div _episodeElement
    C.jQueryFadeIn 50 _div
    return unit


bindEvents :: forall eff. String -> Eff (dom :: J.DOM, trace :: DT.Trace | eff) Unit
bindEvents _linksTemplate = do
    DT.trace "binding events"
    _episodes <- J.select ".day.episodes tr.episode"
    _episodeMoreLink <- J.select ".day.episodes tr.episode a.episode-links"
    J.on "mouseenter" onEpisodeMouseEnter _episodes
    J.on "mouseleave" onEpisodeMouseLeave _episodes
    J.on "click" (onEpisodeLinkClick _linksTemplate) _episodeMoreLink
    return unit


main :: forall eff. Eff (dom :: J.DOM, trace :: DT.Trace | eff) Unit
main = do
    _links <- getUserEpisodeLinks
    bindEvents _links
    return unit

