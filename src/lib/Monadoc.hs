module Monadoc where

import Monadoc.Prelude

import qualified Control.Concurrent.Async as Async
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.List as List
import qualified Data.Pool as Pool
import qualified Monadoc.Vendor.Sql as Sql
import qualified GHC.Conc as Ghc
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Model.Dependency as Dependency
import qualified Monadoc.Model.Distribution as Distribution
import qualified Monadoc.Model.File as File
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Model.HackageUser as HackageUser
import qualified Monadoc.Model.LatestVersion as LatestVersion
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.PreferredVersions as PreferredVersions
import qualified Monadoc.Model.Session as Session
import qualified Monadoc.Model.SourceRepository as SourceRepository
import qualified Monadoc.Model.User as User
import qualified Monadoc.Server.Main as Server
import qualified Monadoc.Server.Settings as Settings
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Flag as Flag
import qualified Monadoc.Type.Version as Version
import qualified Monadoc.Type.Warning as Warning
import qualified Monadoc.Utility.Log as Log
import qualified Monadoc.Worker.Main as Worker
import qualified Paths_monadoc as This
import qualified System.Console.GetOpt as Console
import qualified System.Environment as Environment
import qualified System.Exit as Exit

defaultMain :: IO ()
defaultMain = do
    name <- Environment.getProgName
    arguments <- Environment.getArgs
    mainWith name arguments

mainWith :: String -> [String] -> IO ()
mainWith name arguments = do
    setDefaultExceptionHandler
    context <- getContext name arguments
    Context.withConnection context $ \ connection -> do
        enableWriteAheadLog connection
        Migration.createTable connection
        Migration.runAll connection migrations
    Exception.onException
        (Async.race_ (Server.run context) (Worker.run context))
        . Pool.destroyAllResources $ Context.pool context

setDefaultExceptionHandler :: IO ()
setDefaultExceptionHandler = do
    originalExceptionHandler <- Ghc.getUncaughtExceptionHandler
    Ghc.setUncaughtExceptionHandler
        $ handle originalExceptionHandler
        . defaultExceptionHandler

defaultExceptionHandler :: SomeException -> IO ()
defaultExceptionHandler = Settings.onException Nothing

getContext :: String -> [String] -> IO Context.Context
getContext name arguments = do
    (warnings, config) <- Config.fromArguments arguments

    Monad.forM_ warnings $ \ warning -> do
        Log.warn $ case warning of
            Warning.UnexpectedArgument argument ->
                "unexpected argument " <> show argument
            Warning.UnrecognizedOption option ->
                "unrecognized option " <> show option

    let version = into @String $ into @Version.Version This.version
    Monad.when (Config.help config) $ do
        putStr $ Console.usageInfo (unwords [name, "version", version]) Flag.options
        Exit.exitSuccess

    Monad.when (Config.version config) $ do
        putStrLn version
        Exit.exitSuccess

    Context.fromConfig config

enableWriteAheadLog :: Sql.Connection -> IO ()
enableWriteAheadLog c = Sql.execute_ c "pragma journal_mode = wal"

migrations :: [Migration.Migration]
migrations = List.sortOn Migration.time $ mconcat
    [ Blob.migrations
    , Component.migrations
    , Dependency.migrations
    , Distribution.migrations
    , File.migrations
    , HackageIndex.migrations
    , HackageUser.migrations
    , LatestVersion.migrations
    , Package.migrations
    , PreferredVersions.migrations
    , Session.migrations
    , SourceRepository.migrations
    , User.migrations
    ]
