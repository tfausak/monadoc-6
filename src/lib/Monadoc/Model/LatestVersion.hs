module Monadoc.Model.LatestVersion where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Revision as Revision
import qualified Monadoc.Type.Version as Version
import qualified Monadoc.Vendor.Sql as Sql

type Model = Model.Model LatestVersion

type Key = Key.Key LatestVersion

data LatestVersion = LatestVersion
    { package :: PackageName.PackageName
    , version :: Version.Version
    , revision :: Revision.Revision
    } deriving (Eq, Show)

instance Sql.FromRow LatestVersion where
    fromRow = LatestVersion
        <$> Sql.field
        <*> Sql.field
        <*> Sql.field

instance Sql.ToRow LatestVersion where
    toRow x =
        [ Sql.toField $ package x
        , Sql.toField $ version x
        , Sql.toField $ revision x
        ]

migrations :: [Migration.Migration]
migrations =
    [ Migration.new 2021 6 30 9 23 0
        "create table latestVersion \
        \(key integer not null primary key, \
        \package text not null unique, \
        \version text not null, \
        \revision integer not null)"
    ]

new :: PackageName.PackageName -> Version.Version -> Revision.Revision -> LatestVersion
new = LatestVersion

selectAll :: Sql.Connection -> IO (Map.Map PackageName.PackageName (Version.Version, Revision.Revision))
selectAll connection = do
    rows <- Sql.query_ connection "select package, version, revision from latestVersion"
    pure . Map.fromList $ fmap (\ (p, v, r) -> (p, (v, r))) rows

upsert :: Sql.Connection -> LatestVersion -> IO ()
upsert connection = Sql.execute connection
    "insert into latestVersion (package, version, revision) \
    \values (?, ?, ?) \
    \on conflict (package) \
    \do update set version = excluded.version, \
    \revision = excluded.revision"

deleteByPackage :: Sql.Connection -> PackageName.PackageName -> IO ()
deleteByPackage connection p = Sql.execute connection
    "delete from latestVersion where package = ?" [p]

selectByPackage :: Sql.Connection -> PackageName.PackageName -> IO (Maybe Model)
selectByPackage connection p = fmap Maybe.listToMaybe $ Sql.query connection
    "select key, package, version, revision \
    \from latestVersion \
    \where package = ?"
    [p]
