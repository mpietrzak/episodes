


module Util (
    getElementById,
    parseInt,
    trace
) where


import Fay.Text (Text)

import qualified Fay.Text as T


trace :: Text -> Fay ()
trace = ffi "window.console.log(%1)"


getElementById :: Text -> Fay Element
getElementById = ffi "document.getElementById(%1)"


parseInt :: Text -> Fay Int
parseInt = ffi "window.parseInt(%1)"