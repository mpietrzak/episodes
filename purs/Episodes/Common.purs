
module Episodes.Common
where


import Prelude
import Control.Monad.Eff.Console
import Control.Monad.Eff
import Data.Maybe
import Data.String.Regex (Regex())
import DOM
import qualified Control.Monad.Eff.JQuery as J


-- | PureScript version of Ajax Settings as documented here: http://api.jquery.com/jquery.ajax/#jQuery-ajax-settings
-- Fields are being addded here on as-needed basis.
-- Note that I'm not planning to add success and error callbacks as they're deprecated in favor of Deferred/Promise interface.
data AjaxSettings = AjaxSettings { ajaxData :: Maybe String }


-- | As returned by $.ajax.
foreign import data JQueryXmlHttpRequest :: *


-- | Wraps data in handler in $.ajax callbacks.
foreign import data JQueryXmlHttpData :: *


defaultAjaxSettings :: AjaxSettings
defaultAjaxSettings = AjaxSettings { ajaxData: Nothing }


type JQueryXmlHttpRequestDoneHandler = forall eff. JQueryXmlHttpData -> String -> JQueryXmlHttpRequest -> Eff (dom :: DOM, console :: CONSOLE | eff) Unit

type JQueryXmlHttpRequestFailHandler = forall eff. JQueryXmlHttpRequest -> String -> String -> Eff (dom :: DOM, console :: CONSOLE | eff) Unit


foreign import ajax :: forall eff x. x -> Eff eff JQueryXmlHttpRequest
foreign import getJQueryEventPageX :: forall eff. J.JQueryEvent -> Eff eff Int
foreign import getJQueryEventPageY :: forall eff. J.JQueryEvent -> Eff eff Int
foreign import getValueText :: forall eff. J.JQuery -> Eff (dom :: DOM | eff) String
foreign import is :: forall eff. String -> J.JQuery -> Eff (dom :: DOM | eff) Boolean
foreign import jqXhrDone :: forall eff. JQueryXmlHttpRequest -> JQueryXmlHttpRequestDoneHandler -> Eff (dom :: DOM | eff) JQueryXmlHttpRequest
foreign import jqXhrFail :: forall eff. JQueryXmlHttpRequest -> JQueryXmlHttpRequestFailHandler -> Eff (dom :: DOM | eff) JQueryXmlHttpRequest
foreign import jsonStringify :: forall eff x. x -> Eff eff String
foreign import jQueryFadeIn :: forall eff. Int -> J.JQuery -> Eff (dom :: DOM | eff) J.JQuery
foreign import jQueryFadeOut :: forall eff. Int -> J.JQuery -> Eff (dom :: DOM | eff) J.JQuery
foreign import jQueryFadeOut' :: forall eff eff' a. Int -> Eff eff' a -> J.JQuery -> Eff (dom :: DOM | eff) J.JQuery
foreign import on :: forall eff eff'. String -> (J.JQueryEvent -> J.JQuery -> Eff eff' Unit) -> J.JQuery -> Eff (dom :: DOM | eff) J.JQuery
foreign import redirect :: forall eff. String -> Eff eff Unit
foreign import size  :: J.JQuery -> Int
foreign import split :: Regex -> String -> Array String


-- foreign import split
--     "function split(r) { \
--     \  return function(s) { \
--     \    return s.split(r); \
--     \  }; \
--     \}" :: R.Regex -> String -> [String]

-- foreign import animate
--     "function animate(props) { \
--     \  return function(duration) { \
--     \    return function(ob) { \
--     \      return function() { \
--     \        return ob.animate(props, duration); \
--     \      }; \
--     \    }; \
--     \  }; \
--     \}" :: forall eff css. {| css} -> Number -> J.JQuery -> Eff (dom :: DOM | eff) Unit

-- foreign import split
--     "function split(r) { \
--     \  return function(s) { \
--     \    return s.split(r); \
--     \  }; \
--     \}" :: R.Regex -> String -> [String]


-- | Find user auth id. Search for "#auth-id" input value, return it if found.
getAuthId :: forall eff. Eff (dom :: DOM, console :: CONSOLE | eff) (Maybe String)
getAuthId = do
    _authIdInput <- J.select "#auth-id"
    let _cnt = size _authIdInput
    case _cnt of
            1 -> do
                _authId <- getValueText _authIdInput
                return (Just _authId)
            _ -> return Nothing

