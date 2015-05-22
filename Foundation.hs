{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where


import Control.Applicative ((<$>))
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Database.Persist.Sql (SqlBackend, ConnectionPool, runSqlPool)
import Network.HTTP.Client.Conduit (Manager, HasHttpManager (getHttpManager))
import Prelude
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import Yesod
import Yesod.Auth
import Yesod.Auth.BrowserId
import Yesod.Auth.GoogleEmail2
import Yesod.Core.Types (Logger)
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.PureScript
import Yesod.Static
import Data.Time.Zones (TZ)
import qualified Data.Map as M
import qualified Data.Text as T

import Episodes.Auth (authEpisodes)
import Episodes.DB (getAccountByEmail, checkPassword, createAccount)
import Episodes.Time (NamedTimeZone)
import Model
import Settings (
    appShouldLogAll,
    appStaticDir,
    combineScripts,
    combineStylesheets,
    widgetFile,
    AppSettings (..))
import Settings.StaticFiles


-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings :: AppSettings
    , appStatic :: Static
    , appConnPool :: ConnectionPool
    , appHttpManager :: Manager
    , appLogger :: Logger
    , appCommonTimeZones :: [NamedTimeZone]
    , appCommonTimeZoneMap :: M.Map Text TZ
    , appPureScriptSite :: PureScriptSite
    }

instance HasHttpManager App where
    getHttpManager = appHttpManager

mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . appSettings

    -- Store session data on the client in encrypted cookies.
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        (24 * 60 * 30 * 12)  -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        -- master <- getYesod
        mmsg <- getMessage
        ma <- maybeAuth

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            $(combineStylesheets 'StaticR
                [ css_bootstrap_min_css
                , css_episodes_css ])
            $(combineScripts 'StaticR
               [ js_jquery_2_1_1_js
               , js_bootstrap_min_js ])
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    -- Default to Authorized for now.
    isAuthorized _ _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

    jsLoader _ = BottomOfHeadBlocking


-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool


instance YesodAuth App where
    type AuthId App = AccountId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR

    getAuthId creds = runDB $ do
        let ident = credsIdent creds
        let getter = case T.any ((==) '@') ident of
                True -> getAccountByEmail -- getBy . UniqueAccountEmail . Just
                False -> getBy . UniqueAccountNickname . Just
        _mAccEntity <- getter ident
        case _mAccEntity of
            Just (Entity _id _) -> return $ Just _id
            _ -> do
                now <- liftIO getCurrentTime
                _id <- createAccount ident Nothing now
                return (Just _id)

    authPlugins _app = [ authBrowserId def
                       , authGoogleEmail _id _secret
                       , authEpisodes checkPassword ]
        where
            _id = appGoogleAuthClientId $ appSettings $ _app
            _secret = appGoogleAuthClientSecret $ appSettings $ _app

    authHttpManager = getHttpManager


instance YesodAuthPersist App


instance YesodPureScript App


-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage


