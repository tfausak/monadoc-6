module Monadoc where

import Monadoc.Prelude

import qualified Control.Concurrent.Async as Async
import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Pool as Pool
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as Sql
import qualified GHC.Conc as Ghc
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.PreferredVersions as PreferredVersions
import qualified Monadoc.Model.Session as Session
import qualified Monadoc.Model.User as User
import qualified Monadoc.Server.Main as Server
import qualified Monadoc.Server.Settings as Settings
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Flag as Flag
import qualified Monadoc.Type.Migration as Migration
import qualified Monadoc.Type.Version as Version
import qualified Monadoc.Type.Warning as Warning
import qualified Monadoc.Utility.Log as Log
import qualified Monadoc.Worker.Main as Worker
import qualified Paths_monadoc as This
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
        enableWriteAheadLog connection
        createMigrationTable connection
        traverse_ (runMigration connection) migrations
    Async.race_ (Server.run context) (Worker.run context)

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
enableWriteAheadLog c = Sql.execute_ c $ into @Sql.Query
    "pragma journal_mode = wal"

createMigrationTable :: Sql.Connection -> IO ()
createMigrationTable c = Sql.execute_ c $ into @Sql.Query
    "create table if not exists migration \
    \(migratedAt text not null, \
    \sql text not null, \
    \time text not null primary key) \
    \without rowid"

runMigration :: Sql.Connection -> Migration.Migration -> IO ()
runMigration c m = Sql.withTransaction c $ do
    xs <- Sql.query c (into @Sql.Query "select count(*) from migration where time = ?") [Migration.time m]
    Monad.when (xs /= [Sql.Only (1 :: Int)]) $ do
        Sql.execute_ c $ Migration.sql m
        now <- Time.getCurrentTime
        Sql.execute
            c
            (into @Sql.Query "insert into migration (migratedAt, sql, time) values (?, ?, ?)")
            ( now
            , into @String $ Migration.sql m
            , Migration.time m
            )

migrations :: [Migration.Migration]
migrations = List.sortOn Migration.time $ mconcat
    [ HackageIndex.migrations
    , Package.migrations
    , PreferredVersions.migrations
    , Session.migrations
    , User.migrations
    ]
