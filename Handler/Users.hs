{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Users where


import           Import

import           Control.Applicative ((<*))
import           Yesod.Auth (requireAuthId)
import           Yesod.Form.Bootstrap3
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Time.LocalTime as LT
import qualified Episodes.Time as ET
import qualified Data.Text.Format as TF


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


profileForm :: [(Text, Text)] -> Text -> Html -> MForm Handler (FormResult ProfileForm, Widget)
profileForm tzOpts defaultTimezone = renderBootstrap3 bootstrapFormLayout $ ProfileForm
    <$> areq (selectFieldList tzOpts) (bfs ("Timezone" :: T.Text)) (Just defaultTimezone)
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

    -- we need profile vals from db or defaults
    mEntProfile <- runDB $ getBy $ UniqueProfileUser authId
    $(logDebug) $ TL.toStrict $ TF.format "mProfile: {}" $ TF.Only (show mEntProfile)

    let profileFormTimezone = case mEntProfile of
            Just (Entity _ profile) -> profileTimezone profile
            _ -> "UTC"

    $(logDebug) $ TL.toStrict $ TF.format "profileFormTimezone: {}" [profileFormTimezone]

    (formWidget, formEnctype) <- generateFormPost (profileForm tzOpts profileFormTimezone)
    defaultLayout $ do
        setTitle "Profile"
        $(widgetFile "profile")


postProfileR :: Handler Html
postProfileR = do
    authId <- requireAuthId
    app <- getYesod
    let timezones = commonTimeZones app
    let tzOpts = map timeZoneToTzOpt timezones

    -- we need profile vals from db or defaults
    mEntProfile <- runDB $ getBy $ UniqueProfileUser authId
    let currentTimezone = case mEntProfile of
            Just (Entity _ profile) -> profileTimezone profile
            Nothing -> "UTC"

    ((formResult, formWidget), formEnctype) <- runFormPost (profileForm tzOpts currentTimezone)

    case formResult of
        FormSuccess profileFormValues -> do
            let newProfileTimezone = profileFormTimezone profileFormValues
            let newProfile = case mEntProfile of
                    Just (Entity _ profile) -> profile { profileTimezone = newProfileTimezone }
                    _ -> Profile { profileTimezone = newProfileTimezone
                                 , profileUser = authId }
            -- we either replace if found earlier, or insert if not found earlier
            case mEntProfile of
                Just (Entity profileKey _) -> runDB $ replace profileKey newProfile
                _ -> runDB $ insert_ newProfile
            redirect HomeR
        _ -> defaultLayout $ $(widgetFile "profile")


