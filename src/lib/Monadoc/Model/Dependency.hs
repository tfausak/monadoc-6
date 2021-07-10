{-# LANGUAGE TypeApplications #-}

module Monadoc.Model.Dependency where

import qualified Data.Int as Int
import qualified Distribution.Compat.NonEmptySet as NonEmptySet
import qualified Distribution.Types.Dependency as Cabal
import qualified Distribution.Types.LibraryName as Cabal
import qualified Monadoc.Vendor.Sql as Sql
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.ComponentName as ComponentName
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.VersionRange as VersionRange
import qualified Witch

type Model = Model.Model Dependency

type Key = Key.Key Dependency

-- | This should be unique on 'component', 'packageName', and 'componentName'. A
-- package description can of course define the same dependency multiple times,
-- but those should be merged into a single dependency.
--
-- You might think that the target should be a component rather than a package
-- along with a library name, but unfortunately that doesn't work for two
-- reasons:
--
-- 1.  Dependencies don't have to exist. Any package can depend on any other
--     package, regardless of whether or not the dependency exists. That means
--     the component might not exist in our database.
--
-- 2.  A component is one specific version (really revision) of one part of a
--     package. Dependencies cover version ranges, which can change as new
--     components are added.
data Dependency = Dependency
    { component :: Component.Key
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
    , Migration.new 2021 6 19 17 48 0
        "create index dependency_packageName on dependency (packageName)"
    , Migration.new 2021 6 19 17 49 0
        "create index dependency_libraryName on dependency (libraryName)"
    ]

selectByComponent :: Sql.Connection -> Component.Key -> IO [Model]
selectByComponent connection c = Sql.query
    connection
    "select key, component, packageName, libraryName, versionRange \
    \from dependency \
    \where component = ?"
    [c]

delete :: Sql.Connection -> Key -> IO ()
delete connection key = Sql.execute
    connection
    "delete from dependency where key = ?"
    [key]

insert :: Sql.Connection -> Dependency -> IO Key
insert connection dependency = do
    Sql.execute connection
        "insert into dependency (component, packageName, libraryName, versionRange) \
        \values (?, ?, ?, ?)" dependency
    fmap (Witch.from @Int.Int64) $ Sql.lastInsertRowId connection

fromDependency :: Component.Key -> Cabal.Dependency -> [Dependency]
fromDependency componentKey dependency =
    fmap (fromLibraryName componentKey dependency)
    . NonEmptySet.toList
    $ Cabal.depLibraries dependency

fromLibraryName :: Component.Key -> Cabal.Dependency -> Cabal.LibraryName -> Dependency
fromLibraryName componentKey dependency l = Dependency
    { component = componentKey
    , packageName = Witch.into @PackageName.PackageName $ Cabal.depPkgName dependency
    , libraryName = case l of
        Cabal.LMainLibName -> Witch.via @PackageName.PackageName $ Cabal.depPkgName dependency
        Cabal.LSubLibName n -> Witch.into @ComponentName.ComponentName n
    , versionRange = Witch.into @VersionRange.VersionRange $ Cabal.depVerRange dependency
    }
