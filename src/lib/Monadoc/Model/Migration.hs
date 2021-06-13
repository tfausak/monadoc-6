module Monadoc.Model.Migration where

import Monadoc.Prelude

import qualified Data.Fixed as Fixed
import qualified Data.Maybe as Maybe
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Exception.Mismatch as Mismatch
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model

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
createTable connection = Sql.execute_ connection $ into @Sql.Query
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

selectByTime :: Sql.Connection -> Time.UTCTime -> IO (Maybe (Model.Model Migration))
selectByTime connection time = fmap Maybe.listToMaybe $ Sql.query
    connection
    (into @Sql.Query "select key, migratedAt, sql, time from migration where time = ?")
    [time]

insert :: Sql.Connection -> Migration -> IO Key.Key
insert connection migration = do
    Sql.execute
        connection
        (into @Sql.Query "insert into migration (migratedAt, sql, time) values (?, ?, ?)")
        migration
    fmap (from @Int64) $ Sql.lastInsertRowId connection

run :: Sql.Connection -> Migration -> IO (Model.Model Migration)
run connection migration = Sql.withTransaction connection $ do
    maybeModel <- selectByTime connection $ time migration
    case maybeModel of
        Just model -> do
            let
                actual = sql $ Model.value model
                expected = sql migration
            when (actual /= expected) . throwM $ Mismatch.new expected actual
            pure model
        Nothing -> do
            Sql.execute_ connection . into @Sql.Query $ sql migration
            now <- Time.getCurrentTime
            let newMigration = migration { migratedAt = Just now }
            Sql.execute
                connection
                (into @Sql.Query "insert into migration (migratedAt, sql, time) values (?, ?, ?)")
                newMigration
            key <- fmap (from @Int64) $ Sql.lastInsertRowId connection
            pure Model.Model { Model.key, Model.value = newMigration }
