{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Users where


import           Import

import           Control.Applicative ((<*))
import           Yesod.Auth (requireAuthId)
import           Yesod.Form.Bootstrap3
import qualified Data.Text as T


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


profileForm :: Html -> MForm Handler (FormResult ProfileForm, Widget)
profileForm = renderBootstrap3 bootstrapFormLayout $ ProfileForm
    <$> areq (selectFieldList tzOpts) (bfs ("Timezone" :: T.Text)) Nothing
    <* bootstrapSubmit (BootstrapSubmit ("Save" :: T.Text) "btn btn-default" [])
    where
        tzOpts = [("Europe/Warsaw", "Europe/Warsaw")] :: [(T.Text, T.Text)]


getProfileR :: Handler Html
getProfileR = do
    authId <- requireAuthId
    (formWidget, formEnctype) <- generateFormPost profileForm
    defaultLayout $ do
        $(widgetFile "profile")
