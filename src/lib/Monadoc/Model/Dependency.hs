module Monadoc.Model.Dependency where

import Monadoc.Prelude

import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.ComponentName as ComponentName
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.VersionRange as VersionRange

-- | This should be unique on 'component', 'packageName', and 'componentName'. A
-- package description can of course define the same dependency multiple times,
-- but those should be merged into a single dependency.
data Dependency = Dependency
    { component :: Key.Key
    -- ^ This is the source component.
    , packageName :: PackageName.PackageName
    -- ^ This is the target package.
    , libraryName :: ComponentName.ComponentName
    -- ^ This is a library name. Note that this is always given. If the
    -- dependency is on the default public library, this will be the same as
    -- the package name.
    , versionRange :: VersionRange.VersionRange
    } deriving (Eq, Show)

instance Sql.FromRow Dependency where
    fromRow = Dependency
        <$> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field

instance Sql.ToRow Dependency where
    toRow dependency =
        [ Sql.toField $ component dependency
        , Sql.toField $ packageName dependency
        , Sql.toField $ libraryName dependency
        , Sql.toField $ versionRange dependency
        ]

migrations :: [Migration.Migration]
migrations =
    [ Migration.new 2021 6 15 18 7 0
        "create table dependency \
        \(key integer not null primary key, \
        \component integer not null, \
        \packageName text not null, \
        \libraryName text not null, \
        \versionRange text not null, \
        \unique (component, packageName, libraryName))"
    , Migration.new 2021 6 15 18 11 0
        "create index dependency_component on dependency (component)"
    ]

selectByComponent :: Sql.Connection -> Key.Key -> IO [Model.Model Dependency]
selectByComponent connection component = Sql.query
    connection
    (into @Sql.Query "select key, component, packageName, libraryName, versionRange from dependency where component = ?")
    [component]

deleteByComponent :: Sql.Connection -> Key.Key -> IO ()
deleteByComponent connection component = Sql.execute
    connection
    (into @Sql.Query "delete from dependency where component = ?")
    [component]

insert :: Sql.Connection -> Dependency -> IO Key.Key
insert connection dependency = do
    Sql.execute connection (into @Sql.Query
        "insert into dependency (component, packageName, libraryName, versionRange) \
        \values (?, ?, ?, ?)") dependency
    fmap (from @Int64) $ Sql.lastInsertRowId connection
