{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}


module EpisodeStatus (
    setup
) where


import Fay.Text (Text)
import FFI
import JQuery
import Language.Fay.Yesod
import Prelude
import SharedTypes
import Util (getElementById, parseInt, trace)
import qualified Fay.Text as T


-- Compute element id for episode.
getEpisodeElementId :: Int -> Text
getEpisodeElementId episodeId = T.concat["episode-", T.pack (show episodeId)]


onEpisodeStatusCheckboxResult :: Int -> SetEpisodeStatusResult -> Fay ()
onEpisodeStatusCheckboxResult episodeId r = do
    let ec = setEpisodeStatusResultErrorCode r
    let statusText = setEpisodeStatusResultEpisodeStatus r
    let statusBool = case statusText of
            "seen" -> True
            _ -> False
    setEpisodeStatusCheckboxValue episodeId statusBool


-- Find episode status checkbox, then get value as Bool.
getEpisodeStatusCheckboxValue :: Int -> Fay Bool
getEpisodeStatusCheckboxValue episodeId = do
    episodeElement <- getElementById (getEpisodeElementId episodeId)
    checkbox <- select episodeElement >>= findSelector "input[type=checkbox]"
    v <- is ":checked" checkbox
    return v


-- Find episode status checkbox, then check or uncheck.
setEpisodeStatusCheckboxValue :: Int -> Bool -> Fay ()
setEpisodeStatusCheckboxValue episodeId checked = do
    getElementById (getEpisodeElementId episodeId)
        >>= select
        >>= findSelector "input[type=checkbox]"
        >>= setProp "checked" checked


-- Find episode id for relevant event.
-- Assumes there's a hidden input.episode-id inside closest .episode.
getEventEpisodeId :: Event -> Fay Int
getEventEpisodeId e = target e
                        >>= select
                        >>= closest ".episode"
                        >>= findJQuery ".episode-id"
                        >>= getVal
                        >>= parseInt


onEpisodeStatusCheckboxClick :: Event -> Fay Bool
onEpisodeStatusCheckboxClick e = do
    trace $ T.pack "click"
    episodeId <- getEventEpisodeId e
    -- handlers are stupid: we get new value while inside event handler
    s <- getEpisodeStatusCheckboxValue episodeId
    call (SetEpisodeStatus episodeId s) (onEpisodeStatusCheckboxResult episodeId)
    return False


jqlen :: JQuery -> Fay Int
jqlen = ffi "%1.size()"


setup :: Fay ()
setup = do
    checkboxes <- select ".episode-status.checkbox"
    cnt <- jqlen checkboxes
    trace $ T.concat ["found ", T.pack $ show cnt, " episode status checkboxes"]
    change onEpisodeStatusCheckboxClick checkboxes


