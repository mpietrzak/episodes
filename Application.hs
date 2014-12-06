{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ViewPatterns #-}

module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Thread.Delay (delay)
import Control.Monad.Logger (runLoggingT, runStderrLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Default (def)
import Database.Persist.Sql (SqlBackend, runMigration)
import Database.Persist.Postgresql (createPostgresqlPool, pgConnStr, pgPoolSize)
import Import
import Network.HTTP.Client.Conduit (newManager)
import Network.Wai.Logger (clockDateCacher)
import Network.Wai.Middleware.RequestLogger (mkRequestLogger, outputFormat, OutputFormat (..), IPAddrSource (..), destination)
import System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize)
import Yesod.Auth
import Yesod.Core.Types (loggerSet, Logger (Logger))
import Yesod.Default.Config
import Yesod.Default.Handlers
import Yesod.Default.Main
import Yesod.PureScript
import qualified Data.Map as M
import qualified Database.Persist
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger

import Handler.Calendar
import Handler.Shows
import Handler.Users
import Handler.API (postSetEpisodeStatusR, postSetShowSubscriptionStatusR)
import Handler.Export (
    getExportFileR,
    getExportMainR,
    getICalR,
    getICalPageR)
import Handler.Stats (getStatsR)

import Episodes.Time (NamedTimeZone(..), loadCommonTimezones)
import Episodes.Update (updateTVRageShows)


-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO (Application, LogFunc)
makeApplication conf = do
    foundation <- makeFoundation conf

    -- Initialize the logging middleware
    logWare <- mkRequestLogger def
        { outputFormat =
            if development
                then Detailed True
                else Apache FromSocket
        , destination = RequestLogger.Logger $ loggerSet $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    app <- toWaiAppPlain foundation
    let logFunc = messageLoggerSource foundation (appLogger foundation)
    return (logWare $ defaultMiddlewaresNoLogging app, logFunc)

-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager
    s <- staticSite
    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
              Database.Persist.loadConfig >>=
              Database.Persist.applyEnv

    loggerSet' <- newStdoutLoggerSet defaultBufSize
    (getter, _) <- clockDateCacher

    _timezones <- loadCommonTimezones
    let _timezoneMap = M.fromList $ map (\ntz -> (ntzName ntz, ntzTZ ntz)) _timezones

    let ypso = def { ypsoErrorDivId = Just "main" }
    purs <- createYesodPureScriptSite ypso

    let logger = Yesod.Core.Types.Logger loggerSet' getter
        mkFoundation p = App
            { settings = conf
            , getStatic = s
            , connPool = p
            , httpManager = manager
            , persistConfig = dbconf
            , appLogger = logger
            , commonTimeZones = _timezones
            , commonTimeZoneMap = _timezoneMap
            , getPureScriptSite = purs
            }
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation logger

    p <- flip runLoggingT logFunc
       $ createPostgresqlPool (pgConnStr dbconf) (pgPoolSize dbconf)
    let foundation = mkFoundation p

    -- Perform database migration using our application's logging settings.
    flip runLoggingT logFunc
        (Database.Persist.runPool dbconf (runMigration migrateAll) p)

    let updateInterval = (extraUpdateInterval . appExtra) conf
    let updateCount = extraUpdateCount (appExtra conf)
    _ <- forkIO $ scheduler updateInterval updateCount dbconf p

    return foundation


-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader (fmap fst . makeApplication)
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }


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
            runResourceT $ runStderrLoggingT $ runPool conf updateTVRageShows pool

