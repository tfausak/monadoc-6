module Monadoc.Model.PreferredVersions where

import Monadoc.Prelude

import qualified Data.Maybe as Maybe
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Model.Migration as Migration
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

upsert :: Sql.Connection -> PreferredVersions -> IO Key
upsert connection preferredVersions = do
    Sql.execute
        connection
        (into @Sql.Query
            "insert into preferredVersions (packageName, versionRange) \
            \values (?, ?) \
            \on conflict (packageName) \
            \do update set versionRange = excluded.versionRange")
        preferredVersions
    fmap (from @Int64) $ Sql.lastInsertRowId connection

selectByPackageName :: Sql.Connection -> PackageName.PackageName -> IO (Maybe Model)
selectByPackageName c n = fmap Maybe.listToMaybe $ Sql.query c (into @Sql.Query
    "select key, packageName, versionRange \
    \from preferredVersions \
    \where packageName = ?") [n]
