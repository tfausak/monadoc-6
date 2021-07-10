{-# LANGUAGE TypeApplications #-}

module Monadoc.Main where

import Monadoc.Prelude

import qualified Control.Concurrent.Async as Async
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.Pool as Pool
import qualified Monadoc.Vendor.Sql as Sql
import qualified GHC.Conc as Ghc
import qualified Monadoc.Data.Migrations as Migrations
import qualified Monadoc.Model.Migration as Migration
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
        Migration.runAll connection Migrations.migrations
    Exception.onException
        (Async.race_ (Server.run context) (Worker.run context))
        . Pool.destroyAllResources $ Context.pool context

setDefaultExceptionHandler :: IO ()
setDefaultExceptionHandler = do
    originalExceptionHandler <- Ghc.getUncaughtExceptionHandler
    Ghc.setUncaughtExceptionHandler
        $ Exception.handle originalExceptionHandler
        . defaultExceptionHandler

defaultExceptionHandler :: Exception.SomeException -> IO ()
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
