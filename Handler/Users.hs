{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Users where


import           Import

import           Control.Applicative ((<*))
import           Data.Time (getCurrentTime)
import           Yesod.Auth (requireAuthId)
import           Yesod.Form.Bootstrap3
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Episodes.Time as ET
import qualified Data.Text.Format as TF


data ProfileForm = ProfileForm { profileFormTimezone :: Text }


data RegisterForm = RegisterForm { registerFormEmail :: Text
                                 , registerFormPassword :: Text }


-- data PasswordForm = PasswordForm { passwordFormPassword :: Text }


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


registerForm :: Form RegisterForm
registerForm = renderBootstrap3 bootstrapFormLayout $ RegisterForm
    <$> areq textField (bfs ("Email" :: Text)) Nothing
    <*> areq passwordField (bfs ("Password" :: Text)) Nothing
    <* bootstrapSubmit (BootstrapSubmit ("Register" :: T.Text) "btn btn-default" [])


-- passwordForm :: Form PasswordForm
-- passwordForm = renderBootstrap3 bootstrapFormLayout $ PasswordForm
--     <$> areq passwordField (bfs ("Password" :: Text)) Nothing
--     <* bootstrapSubmit (BootstrapSubmit ("Set Password" :: T.Text) "btn btn-default" [])


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
    mEntProfile <- runDB $ getBy $ UniqueProfileAccount authId
    $(logDebug) $ TL.toStrict $ TF.format "mProfile: {}" $ TF.Only (show mEntProfile)

    let currentTimezone = case mEntProfile of
            Just (Entity _ profile) -> maybe "UTC" id $ profileTimezone profile
            _ -> "UTC"

    (formWidget, formEnctype) <- generateFormPost (profileForm tzOpts currentTimezone)
    -- (passFormWidget, passFormEnctype) <- generateFormPost passwordForm
    defaultLayout $ do
        setTitle "Profile"
        $(widgetFile "profile")


postProfileR :: Handler Html
postProfileR = do
    authId <- requireAuthId
    app <- getYesod
    now <- liftIO getCurrentTime
    let timezones = commonTimeZones app
    let tzOpts = map timeZoneToTzOpt timezones

    -- we need profile vals from db or defaults
    mEntProfile <- runDB $ getBy $ UniqueProfileAccount authId
    let currentTimezone = case mEntProfile of
            Just (Entity _ profile) -> maybe "UTC" id $ profileTimezone profile
            Nothing -> "UTC"

    ((formResult, formWidget), formEnctype) <- runFormPost (profileForm tzOpts currentTimezone)

    case formResult of
        FormSuccess profileFormValues -> do
            let newProfileTimezone = profileFormTimezone profileFormValues
            let newProfile = case mEntProfile of
                    Just (Entity _ profile) -> profile { profileTimezone = Just newProfileTimezone
                                                       , profileModified = now }
                    _ -> Profile { profileTimezone = Just newProfileTimezone
                                 , profileAccount = authId
                                 , profileCreated = now
                                 , profileModified = now }
            -- we either replace if found earlier, or insert if not found earlier
            case mEntProfile of
                Just (Entity profileKey _) -> runDB $ replace profileKey newProfile
                _ -> runDB $ insert_ newProfile
            redirect HomeR
        _ -> do
            -- (passFormWidget, passFormEnctype) <- generateFormPost passwordForm
            defaultLayout $ do
                setTitle "Profile"
                $(widgetFile "profile")


-- getRegisterR :: Handler Html
-- getRegisterR = do
--     (formWidget, formEnctype) <- generateFormPost registerForm
--     defaultLayout $ do
--         setTitle "Register Account"
--         $(widgetFile "register")


-- postRegisterR :: Handler Html
-- postRegisterR = do
--     ((formResult, formWidget), formEnctype) <- runFormPost registerForm
--
--     case formResult of
--         FormSuccess registerFormValues -> do
--             let email = registerFormEmail registerFormValues
--             let password = registerFormPassword registerFormValues
--             u <- setPassword password $ Account { accountPassword = Nothing, accountAdmin = False, accountNickname = Nothing, accountEmail = Just email }
--             _ <- runDB $ insert_ u
--             redirect HomeR
--         _ -> defaultLayout $ $(widgetFile "register")


-- postProfilePasswordR :: Handler Html
-- postProfilePasswordR = do
--     auth <- requireAuth
--     authId <- requireAuthId
--
--     -- :'(
--     app <- getYesod
--     let timezones = commonTimeZones app
--     let tzOpts = map timeZoneToTzOpt timezones
--     mEntProfile <- runDB $ getBy $ UniqueProfileAccount authId
--     let curTimezone = case mEntProfile of
--             Just (Entity _ profile) -> maybe "UTC" id $ profileTimezone profile
--             _ -> "UTC"
--     (formWidget, formEnctype) <- generateFormPost (profileForm tzOpts curTimezone)
--
--     ((passFormResult, passFormWidget), passFormEnctype) <- runFormPost passwordForm
--     case passFormResult of
--         FormSuccess passFormValues -> do
--             -- let password = passwordFormPassword passFormValues
--             let nu = entityVal auth
--             runDB $ replace authId nu
--             setMessage "Password changed"
--             redirect ProfileR
--         _ -> defaultLayout $ do
--             setTitle "Profile"
--             $(widgetFile "profile")

