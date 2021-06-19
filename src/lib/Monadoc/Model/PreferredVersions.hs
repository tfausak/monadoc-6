module Monadoc.Model.PreferredVersions where

import Monadoc.Prelude

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Monadoc.Vendor.Sql as Sql
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.VersionRange as VersionRange

type Model = Model.Model PreferredVersions

type Key = Key.Key PreferredVersions

data PreferredVersions = PreferredVersions
    { packageName :: PackageName.PackageName
    , versionRange :: VersionRange.VersionRange
    } deriving (Eq, Show)

instance Sql.FromRow PreferredVersions where
    fromRow = PreferredVersions
        <$> Sql.field
        <*> Sql.field

instance Sql.ToRow PreferredVersions where
    toRow preferredVersions =
        [ Sql.toField $ packageName preferredVersions
        , Sql.toField $ versionRange preferredVersions
        ]

migrations :: [Migration.Migration]
migrations =
    [ Migration.new 2021 6 2 21 34 0
        "create table preferredVersions \
        \(key integer not null primary key, \
        \packageName text not null, \
        \versionRange text not null)"
    , Migration.new 2021 6 12 9 17 0
        "create unique index preferredVersions_packageName on preferredVersions (packageName)"
    ]

new :: PackageName.PackageName -> VersionRange.VersionRange -> PreferredVersions
new = PreferredVersions

upsert :: Sql.Connection -> PreferredVersions -> IO ()
upsert connection preferredVersions = do
    Sql.execute
        connection
            "insert into preferredVersions (packageName, versionRange) \
            \values (?, ?) \
            \on conflict (packageName) \
            \do update set versionRange = excluded.versionRange"
        preferredVersions

selectByPackageName :: Sql.Connection -> PackageName.PackageName -> IO (Maybe Model)
selectByPackageName c n = fmap Maybe.listToMaybe $ Sql.query c
    "select key, packageName, versionRange \
    \from preferredVersions \
    \where packageName = ?" [n]

selectAll :: Sql.Connection -> IO (Map PackageName.PackageName VersionRange.VersionRange)
selectAll connection = do
    rows <- Sql.query connection "select packageName, versionRange from preferredVersions" ()
    pure $ Map.fromList rows
