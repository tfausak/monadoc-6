module Monadoc.Model.File where

import Monadoc.Prelude

import qualified Monadoc.Model.Distribution as Distribution
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.Sha256 as Sha256
import qualified Monadoc.Vendor.Sql as Sql

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

upsert :: Sql.Connection -> File -> IO ()
upsert connection = Sql.execute connection
    "insert into file (distribution, hash, path) \
    \values (?, ?, ?) \
    \on conflict (distribution, path) \
    \do update set hash = excluded.hash"
