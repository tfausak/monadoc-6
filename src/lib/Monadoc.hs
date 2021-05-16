module Monadoc where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.Version as Version
import qualified GHC.Conc as Ghc
import qualified Monadoc.Server.Application as Application
import qualified Monadoc.Server.Middleware as Middleware
import qualified Monadoc.Server.Settings as Settings
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Flag as Flag
import qualified Monadoc.Type.Warning as Warning
import qualified Network.Wai.Handler.Warp as Warp
import qualified Paths_monadoc as Package
import qualified System.Console.GetOpt as Console
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO

main :: IO ()
main = do
    name <- Environment.getProgName
    arguments <- Environment.getArgs
    mainWith name arguments

mainWith :: String -> [String] -> IO ()
mainWith name arguments = do
    setDefaultExceptionHandler
    config <- getConfig name arguments
    Warp.runSettings (Settings.fromConfig config)
        $ Middleware.middleware Application.application

setDefaultExceptionHandler :: IO ()
setDefaultExceptionHandler = do
    originalHandler <- Ghc.getUncaughtExceptionHandler
    Ghc.setUncaughtExceptionHandler
        $ Exception.handle originalHandler
        . defaultHandler

defaultHandler :: Exception.SomeException -> IO ()
defaultHandler = IO.hPutStrLn IO.stderr . Exception.displayException

getConfig :: String -> [String] -> IO Config.Config
getConfig name arguments = do
    (warnings, config) <- Config.fromArguments arguments

    Monad.forM_ warnings $ \ warning -> do
        IO.hPutStrLn IO.stderr $ case warning of
            Warning.UnexpectedArgument argument ->
                "WARNING: unexpected argument " <> show argument
            Warning.UnrecognizedOption option ->
                "WARNING: unrecognized option " <> show option

    let version = Version.showVersion Package.version
    Monad.when (Config.help config) $ do
        putStr $ Console.usageInfo (unwords [name, "version", version]) Flag.options
        Exit.exitSuccess

    Monad.when (Config.version config) $ do
        putStrLn version
        Exit.exitSuccess

    pure config
