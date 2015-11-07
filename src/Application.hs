{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Application (
    appMain,
    develMain,
    getApplicationDev,
    makeApplication,
    makeFoundation,
    shutdownApp
) where

import ClassyPrelude.Yesod
import Control.Concurrent (forkIO)
import Control.Concurrent.Thread.Delay (delay)
import Control.Monad.Logger (liftLoc, runLoggingT, runStderrLoggingT)
import Database.Persist.Postgresql          (createPostgresqlPool, pgConnStr,
                                             pgPoolSize, runSqlPool)
import Language.Haskell.TH.Syntax           (qLocation)
import Network.Wai.Handler.Warp             (Settings, defaultSettings,
                                             defaultShouldDisplayException,
                                             runSettings, setHost,
                                             setOnException, setPort)
import Network.Wai.Middleware.RequestLogger (Destination (Logger),
                                             IPAddrSource (..),
                                             OutputFormat (..), destination,
                                             mkRequestLogger, outputFormat)
import System.Log.FastLogger                (defaultBufSize, newStdoutLoggerSet,
                                             toLogStr)
import Yesod.Auth
import Yesod.Core.Types (loggerSet)
import Yesod.Default.Config2
import Yesod.Default.Handlers
-- import Yesod.PureScript
import qualified Data.Map as M

import Foundation
import Episodes.Handler.About (getAboutR)
import Episodes.Handler.Admin (getAdminR)
import Episodes.Handler.Calendar
import Episodes.Handler.Shows
import Episodes.Handler.ShowChanges (
    getShowChangesAddEpisodesR,
    getShowChangesAddSeasonsR,
    getShowChangesDeleteEpisodeR,
    getShowChangesDeleteSeasonR,
    getShowChangesEditEpisodeR,
    getShowChangesEditSeasonR,
    getShowSubmitChangesR,
    postShowChangesAddEpisodesR,
    postShowChangesAddSeasonsR,
    postShowChangesDeleteEpisodeR,
    postShowChangesDeleteSeasonR,
    postShowChangesEditEpisodeR,
    postShowChangesEditSeasonR
    )
import Episodes.Handler.ShowEdit (
    getShowEditAddEpisodesR,
    getShowEditAddSeasonsR,
    getShowEditDeleteEpisodeR,
    getShowEditDeleteSeasonR,
    getEditEpisodeR,
    getShowEditEditSeasonR,
    getShowEditR,
    postShowEditAddEpisodesR,
    postShowEditAddSeasonsR,
    postShowEditDeleteEpisodeR,
    postShowEditDeleteSeasonR,
    postEditEpisodeR,
    postShowEditEditSeasonR )
import Episodes.Handler.Users
import Episodes.Handler.API (
    postSetEpisodeStatusR,
    postSetSeasonCollapseR,
    postSetShowSubscriptionStatusR)
import Episodes.Handler.Export (
    getExportFileR,
    getExportMainR,
    getICalR,
    getICalPageR)
import Episodes.Handler.Stats (getStatsR)
import Episodes.StaticFiles (episodesStatic)
import Model
import Settings

import Episodes.Time (NamedTimeZone(..), loadCommonTimezones)
import Episodes.Update (updateTVRageShows)


-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp


makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
    appHttpManager <- newManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger

    -- appStatic <-
    --     (if appMutableStatic appSettings then staticDevel else static)
    --     (appStaticDir appSettings)

    let appStatic = episodesStatic

    -- appPureScriptSite <- createYesodPureScriptSite yesodPureScriptOptions

    appCommonTimeZones <- loadCommonTimezones
    let appCommonTimeZoneMap = M.fromList $ map (\ntz -> (ntzName ntz, ntzTZ ntz)) appCommonTimeZones

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation appConnPool = App {..}
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation appLogger

    -- Create the database connection pool
    pool <- flip runLoggingT logFunc $ createPostgresqlPool
        (pgConnStr  $ appDatabaseConf appSettings)
        (pgPoolSize $ appDatabaseConf appSettings)

    -- Perform database migration using our application's logging settings.
    runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc

    -- Start scheduler
    -- let updateInterval = appUpdateInterval appSettings
    -- let updateCount = appUpdateCount appSettings
    -- _ <- forkIO $ scheduler updateInterval updateCount (appDatabaseConf appSettings) pool

    -- Return the foundation
    return $ mkFoundation pool


-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applyng some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- mkRequestLogger def
        { outputFormat =
            if appDetailedRequestLogging $ appSettings foundation
                then Detailed True
                else Apache
                        (if appIpFromHeader $ appSettings foundation
                            then FromFallback
                            else FromSocket)
        , destination = Logger $ loggerSet $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    return $ logWare $ defaultMiddlewaresNoLogging appPlain


-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
      setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (appLogger foundation)
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings


-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    return (wsettings, app)


getAppSettings :: IO AppSettings
getAppSettings = loadAppSettings [configSettingsYml] [] useEnv


-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from all relevant sources
    settings <- loadAppSettingsArgs
        -- fall back to compile-time values, set to [] to require values at runtime
        [configSettingsYmlValue]

        -- allow environment variables to override
        useEnv

    -- Generate the foundation from the settings
    foundation <- makeFoundation settings

    -- Generate a WAI Application from the foundation
    app <- makeApplication foundation

    -- Run the application with Warp
    runSettings (warpSettings foundation) app


shutdownApp :: App -> IO ()
shutdownApp _ = return ()


-- | Code that runs jobs.
-- This thread should never stop.
-- Jobs are fired in separate threads.
-- Uses DB pool.
scheduler :: (PersistConfig c, PersistConfigBackend c ~ ReaderT SqlBackend)
          => Int -> Int -> c -> PersistConfigPool c -> IO ()
scheduler _interval _count conf pool = loop
    where
        loop = do
            _ <- forkIO job
            _ <- delay $ fromIntegral $ _interval * 1000 * 1000
            loop
        job = do
            runResourceT $ runStderrLoggingT $ runPool conf (updateTVRageShows _count) pool

