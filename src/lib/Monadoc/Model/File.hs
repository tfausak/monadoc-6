module Monadoc.Model.File where

import Monadoc.Prelude

import qualified Data.Maybe as Maybe
import qualified Monadoc.Model.Distribution as Distribution
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Sha256 as Sha256
import qualified Monadoc.Vendor.Sql as Sql

type Model = Model.Model File

type Key = Key.Key File

data File = File
    { distribution :: Distribution.Key
    , hash :: Sha256.Sha256
    , path :: FilePath
    } deriving (Eq, Show)

instance Sql.FromRow File where
    fromRow = File
        <$> Sql.field
        <*> Sql.field
        <*> Sql.field

instance Sql.ToRow File where
    toRow file =
        [ Sql.toField $ distribution file
        , Sql.toField $ hash file
        , Sql.toField $ path file
        ]

migrations :: [Migration.Migration]
migrations =
    [ Migration.new 2021 6 24 7 20 0
        "create table file \
        \(key integer not null primary key, \
        \distribution integer not null, \
        \hash text not null, \
        \path text not null, \
        \unique (distribution, path))"
    ]

select :: Sql.Connection -> Key -> IO (Maybe Model)
select connection key = do
    rows <- Sql.query
        connection
        "select key, distribution, hash, path \
        \from file \
        \where key = ?"
        [key]
    pure $ Maybe.listToMaybe rows

selectByDistribution :: Sql.Connection -> Distribution.Key -> IO [Model]
selectByDistribution connection d = Sql.query
    connection
    "select key, distribution, hash, path \
    \from file \
    \where distribution = ?"
    [d]

selectByDistributionAndPath :: Sql.Connection -> Distribution.Key -> FilePath -> IO (Maybe Model)
selectByDistributionAndPath connection d p = fmap Maybe.listToMaybe $ Sql.query
    connection
    "select key, distribution, hash, path \
    \from file \
    \where distribution = ? \
    \and path = ?"
    (d, p)

upsert :: Sql.Connection -> File -> IO ()
upsert connection = Sql.execute connection
    "insert into file (distribution, hash, path) \
    \values (?, ?, ?) \
    \on conflict (distribution, path) \
    \do update set hash = excluded.hash"
