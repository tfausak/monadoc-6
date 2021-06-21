module Monadoc.Model.Distribution where

import Monadoc.Prelude

import qualified Data.Map as Map
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Sha256 as Sha256
import qualified Monadoc.Type.Version as Version
import qualified Monadoc.Vendor.Sql as Sql

type Model = Model.Model Distribution

type Key = Key.Key Distribution

data Distribution = Distribution
    { hash :: Sha256.Sha256
    , package :: PackageName.PackageName
    , version :: Version.Version
    } deriving (Eq, Show)

instance Sql.FromRow Distribution where
    fromRow = Distribution
        <$> Sql.field
        <*> Sql.field
        <*> Sql.field

instance Sql.ToRow Distribution where
    toRow distribution =
        [ Sql.toField $ hash distribution
        , Sql.toField $ package distribution
        , Sql.toField $ version distribution
        ]

migrations :: [Migration.Migration]
migrations =
    [ Migration.new 2021 6 20 17 52 0
        "create table distribution \
        \(key integer not null primary key, \
        \hash text not null, \
        \package text not null, \
        \version text not null, \
        \unique (package, version))"
    ]

selectHashes :: Sql.Connection -> IO (Map (PackageName.PackageName, Version.Version) Sha256.Sha256)
selectHashes connection = do
    rows <- Sql.query_ connection
        "select package, version, hash from distribution group by package, version"
    pure . Map.fromList $ fmap (\ (p, v, h) -> ((p, v), h)) rows

upsert :: Sql.Connection -> Distribution -> IO ()
upsert connection = Sql.execute connection
    "insert into distribution (hash, package, version) \
    \values (?, ?, ?) \
    \on conflict (package, version) \
    \do update set hash = excluded.hash"
