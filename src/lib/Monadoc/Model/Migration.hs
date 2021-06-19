module Monadoc.Model.Migration where

import Monadoc.Prelude

import qualified Data.Fixed as Fixed
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Time as Time
import qualified Monadoc.Vendor.Sql as Sql
import qualified Monadoc.Exception.Mismatch as Mismatch
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Utility.Foldable as Foldable

type Model = Model.Model Migration

type Key = Key.Key Migration

data Migration = Migration
    { migratedAt :: Maybe Time.UTCTime
    , sql :: Text
    , time :: Time.UTCTime
    } deriving (Eq, Show)

instance Sql.FromRow Migration where
    fromRow = Migration
        <$> Sql.field
        <*> Sql.field
        <*> Sql.field

instance Sql.ToRow Migration where
    toRow migration =
        [ Sql.toField $ migratedAt migration
        , Sql.toField $ sql migration
        , Sql.toField $ time migration
        ]

createTable :: Sql.Connection -> IO ()
createTable connection = Sql.execute_ connection
    "create table if not exists migration \
    \(key integer not null primary key, \
    \migratedAt text, \
    \sql text not null, \
    \time text not null unique)"

new
    :: Time.Year
    -> Time.MonthOfYear
    -> Time.DayOfMonth
    -> Int
    -> Int
    -> Fixed.Pico
    -> String
    -> Migration
new year month day hour minute sec q = Migration
    { migratedAt = Nothing
    , sql = into @Text q
    , time = Time.UTCTime (Time.fromGregorian year month day)
        . Time.timeOfDayToTime
        $ Time.TimeOfDay hour minute sec
    }

selectAll :: Sql.Connection -> IO [Model]
selectAll connection =
    Sql.query connection "select key, migratedAt, sql, time from migration" ()

selectByTime :: Sql.Connection -> Time.UTCTime -> IO (Maybe Model)
selectByTime connection time = fmap Maybe.listToMaybe $ Sql.query
    connection
    "select key, migratedAt, sql, time from migration where time = ?"
    [time]

insert :: Sql.Connection -> Migration -> IO Key
insert connection migration = do
    Sql.execute
        connection
        "insert into migration (migratedAt, sql, time) values (?, ?, ?)"
        migration
    fmap (from @Int64) $ Sql.lastInsertRowId connection

runAll :: Sql.Connection -> [Migration] -> IO ()
runAll connection toRun = do
    migrations <- selectAll connection
    let migrationsByTime = Foldable.indexBy (time . Model.value) migrations
    traverse_ (runOne connection migrationsByTime) toRun

runOne :: Sql.Connection -> Map Time.UTCTime Model -> Migration -> IO ()
runOne connection migrations migration =
    case Map.lookup (time migration) migrations of
        Just model -> do
            let
                actual = sql $ Model.value model
                expected = sql migration
            when (actual /= expected) . throwM $ Mismatch.new expected actual
        Nothing -> Sql.withTransaction connection $ do
            Sql.execute_ connection . into @String $ sql migration
            now <- Time.getCurrentTime
            let newMigration = migration { migratedAt = Just now }
            Sql.execute
                connection
                "insert into migration (migratedAt, sql, time) values (?, ?, ?)"
                newMigration
