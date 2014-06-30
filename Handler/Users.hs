{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Users where


import           Import

import           Control.Applicative ((<*))
import           Yesod.Auth (requireAuthId)
import           Yesod.Form.Bootstrap3
import qualified Data.Text as T
import qualified Data.Time.LocalTime as LT
import qualified Episodes.Time as ET


data ProfileForm = ProfileForm { profileFormTimezone :: Text }


textFieldSettings :: Text -> Text -> FieldSettings site
textFieldSettings labelText placeholderText =
    withPlaceholder placeholderText $ bfs labelText


bootstrapFormLayout :: BootstrapFormLayout
bootstrapFormLayout = BootstrapHorizontalForm labelOffset labelSize inputOffset inputSize
    where
        labelOffset = ColSm 1
        labelSize = ColSm 2
        inputOffset = ColSm 0
        inputSize = ColSm 6


profileForm :: [(Text, Text)] -> Html -> MForm Handler (FormResult ProfileForm, Widget)
profileForm tzOpts = renderBootstrap3 bootstrapFormLayout $ ProfileForm
    <$> areq (selectFieldList tzOpts) (bfs ("Timezone" :: T.Text)) Nothing
    <* bootstrapSubmit (BootstrapSubmit ("Save" :: T.Text) "btn btn-default" [])


timeZoneToTzOpt :: ET.NamedTimeZone -> (Text, Text)
timeZoneToTzOpt ntz = (nt, nt)
    where
        nt = ET.ntzName ntz


getProfileR :: Handler Html
getProfileR = do
    authId <- requireAuthId
    app <- getYesod
    let timezones = commonTimeZones app
    let tzOpts = map timeZoneToTzOpt timezones
    (formWidget, formEnctype) <- generateFormPost (profileForm tzOpts)
    defaultLayout $ do
        $(widgetFile "profile")


