
module Common
where


import Prelude hiding (show)
import Control.Monad.Eff
import Data.Maybe
import Debug.Trace
import DOM
import qualified Control.Monad.JQuery as J
import qualified Data.String.Regex as R


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


type JQueryXmlHttpRequestDoneHandler = forall eff. JQueryXmlHttpData -> String -> JQueryXmlHttpRequest -> Eff (dom :: DOM, trace :: Trace | eff) {}

type JQueryXmlHttpRequestFailHandler = forall eff. JQueryXmlHttpRequest -> String -> String -> Eff (dom :: DOM, trace :: Trace | eff) {}


foreign import closest
    "function closest(sel) { \
    \  return function(ob) { \
    \    return function() { \
    \      return ob.closest(sel); \
    \    } \
    \  }; \
    \}" :: forall eff. String -> J.JQuery -> Eff (dom :: DOM | eff) J.JQuery


foreign import find
    "function find(sel) { \
    \  return function(ob) { \
    \    return function() { \
    \      return ob.find(sel); \
    \    }; \
    \  }; \
    \}" :: forall eff. String -> J.JQuery -> Eff (dom :: DOM | eff) J.JQuery


foreign import parent
    "function parent(ob) { \
    \  return function() { \
    \    return ob.parent(); \
    \  }; \
    \}" :: forall eff. J.JQuery -> Eff (dom :: DOM | eff) J.JQuery


foreign import jQueryShow
    "function jQueryShow(duration) { \
    \  return function(ob) { \
    \    return function() { \
    \      return ob.show(duration); \
    \    }; \
    \  }; \
    \}" :: forall eff. Number -> J.JQuery -> Eff (dom :: DOM | eff) J.JQuery


foreign import jQueryHide
    "function jQueryHide(duration) { \
    \  return function(ob) { \
    \    return function() { \
    \      return ob.hide(duration); \
    \    }; \
    \  }; \
    \}" :: forall eff. Number -> J.JQuery -> Eff (dom :: DOM | eff) J.JQuery


foreign import jQueryFadeIn
    "function jQueryFadeIn(duration) { \
    \  return function(ob) { \
    \    return function() { \
    \      return ob.fadeIn(duration); \
    \    }; \
    \  }; \
    \}" :: forall eff. Number -> J.JQuery -> Eff (dom :: DOM | eff) J.JQuery


foreign import jQueryFadeOut'
    "function jQueryFadeOut$prime(duration) { \
    \  return function(act) { \
    \    return function(ob) { \
    \      return function() { \
    \        return ob.fadeOut(duration, act); \
    \      }; \
    \    }; \
    \  }; \
    \}" :: forall eff a. Number -> (forall e. Eff (dom :: DOM | e) a) -> J.JQuery -> Eff (dom :: DOM | eff) J.JQuery


foreign import animate
    "function animate(props) { \
    \  return function(duration) { \
    \    return function(ob) { \
    \      return function() { \
    \        return ob.animate(props, duration); \
    \      }; \
    \    }; \
    \  }; \
    \}" :: forall eff css. {| css} -> Number -> J.JQuery -> Eff (dom :: DOM | eff) Unit


foreign import is
    "function is(sel) { \
    \  return function(ob) { \
    \    return function() { \
    \      return ob.is(sel); \
    \    }; \
    \  }; \
    \}" :: forall eff. String -> J.JQuery -> Eff (dom :: DOM | eff) Boolean


-- Get the value of a text field as String
foreign import getValueText
    "function getValueText(ob) { \
    \  return function() { \
    \    return (ob.val() || ''); \
    \  }; \
    \}" :: forall eff. J.JQuery -> Eff (dom :: DOM | eff) String


foreign import jsonStringify
    "function jsonStringify(v) { \
    \  return function() { \
    \    return JSON.stringify(v, null, \"    \"); \
    \  }; \
    \}" :: forall eff x. x -> Eff eff String


-- Send JQuery Ajax
foreign import ajax
    "function ajax(settings) { \
    \  return function() { \
    \    return jQuery.ajax(settings); \
    \  }; \
    \}" :: forall eff css. { | css } -> Eff eff JQueryXmlHttpRequest


-- Add .done to jqXHR
foreign import jqXhrDone
    "function jqXhrDone(req) { \
    \  return function(act) { \
    \    return function() { \
    \      return req.done(function(data, status, req) { \
    \        act(data)(status)(req)(); \
    \      }); \
    \    }; \
    \  }; \
    \}" :: forall eff. JQueryXmlHttpRequest -> JQueryXmlHttpRequestDoneHandler -> Eff eff JQueryXmlHttpRequest


-- Add .fail to jqXHR
foreign import jqXhrFail
    "function jqXhrFail(req) { \
    \  return function(act) { \
    \    return function() { \
    \      return req.fail(function(req, status, err) { \
    \        act(req)(status)(err)(); \
    \      }); \
    \    }; \
    \  }; \
    \}" :: forall eff. JQueryXmlHttpRequest -> JQueryXmlHttpRequestFailHandler -> Eff eff JQueryXmlHttpRequest


foreign import getJQueryEventPageX
    "function getJQueryEventPageX(e) { \
    \  return function() { \
    \    return e.pageX; \
    \  }; \
    \}" :: forall eff. J.JQueryEvent -> Eff eff Number


foreign import getJQueryEventPageY
    "function getJQueryEventPageY(e) { \
    \  return function() { \
    \    return e.pageY; \
    \  }; \
    \}" :: forall eff. J.JQueryEvent -> Eff eff Number


foreign import split
    "function split(r) { \
    \  return function(s) { \
    \    return s.split(r); \
    \  }; \
    \}" :: R.Regex -> String -> [String]


foreign import on
  """
  function on(evt) {
    return function(act) {
      return function(ob) {
        return function() {
          return ob.on(evt, function(e) {
            act(e)(jQuery(this))();
          });
        };
      };
    };
  }
  """ :: forall eff eff' a. String -> (J.JQueryEvent -> J.JQuery -> Eff eff' a) -> J.JQuery -> Eff (dom :: DOM | eff) J.JQuery




