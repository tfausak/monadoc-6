module Monadoc.Model.PreferredVersions where

import Monadoc.Prelude

import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Type.Migration as Migration
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.VersionRange as VersionRange

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
        \(packageName text not null primary key, \
        \versionRange text not null) \
        \without rowid"
    ]

new :: PackageName.PackageName -> VersionRange.VersionRange -> PreferredVersions
new = PreferredVersions

upsert :: Sql.Connection -> PreferredVersions -> IO ()
upsert c = Sql.execute c $ into @Sql.Query
    "insert into preferredVersions (packageName, versionRange) \
    \values (?, ?) \
    \on conflict (packageName) \
    \do update set versionRange = excluded.versionRange"
