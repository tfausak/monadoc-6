module Monadoc where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.Pool as Pool
import qualified Database.SQLite.Simple as Sql
import qualified GHC.Conc as Ghc
import qualified Monadoc.Server.Application as Application
import qualified Monadoc.Server.Middleware as Middleware
import qualified Monadoc.Server.Settings as Settings
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Flag as Flag
import qualified Monadoc.Type.Warning as Warning
import qualified Monadoc.Utility.Convert as Convert
import qualified Monadoc.Utility.Log as Log
import qualified Network.Wai.Handler.Warp as Warp
import qualified Paths_monadoc as Package
import qualified System.Console.GetOpt as Console
import qualified System.Environment as Environment
import qualified System.Exit as Exit

main :: IO ()
main = do
    name <- Environment.getProgName
    arguments <- Environment.getArgs
    mainWith name arguments

mainWith :: String -> [String] -> IO ()
mainWith name arguments = do
    setDefaultExceptionHandler
    context <- getContext name arguments
    Pool.withResource (Context.pool context) $ \ connection -> do
        rows <- Sql.query_ connection $ Convert.stringToQuery "select 1"
        Monad.guard $ rows == [[1 :: Int]]
    Warp.runSettings (Settings.fromConfig $ Context.config context)
        $ Middleware.middleware Application.application

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

    let version = Convert.versionToString Package.version
    Monad.when (Config.help config) $ do
        putStr $ Console.usageInfo (unwords [name, "version", version]) Flag.options
        Exit.exitSuccess

    Monad.when (Config.version config) $ do
        putStrLn version
        Exit.exitSuccess

    Context.fromConfig config
