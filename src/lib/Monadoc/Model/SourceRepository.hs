module Monadoc.Model.SourceRepository where

import Monadoc.Prelude

import qualified Monadoc.Vendor.Sql as Sql
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.RepositoryKind as RepositoryKind
import qualified Monadoc.Type.RepositoryType as RepositoryType

type Model = Model.Model SourceRepository

type Key = Key.Key SourceRepository

data SourceRepository = SourceRepository
    { branch :: Maybe String
    -- ^ Almost always null, sometimes "master".
    , kind :: RepositoryKind.RepositoryKind
    -- ^ Almost always "head", rarely "this", nothing else.
    , location :: Maybe String
    -- ^ Amazingly this is in fact null sometimes.
    , module_ :: Maybe String
    -- ^ This appears to always be null.
    , package :: Package.Key
    , subdir :: Maybe FilePath
    -- ^ Almost always null.
    , tag :: Maybe String
    -- ^ Almost always null.
    , type_ :: Maybe RepositoryType.RepositoryType
    -- ^ Never null. Almost always "git", but sometimes: "darcs", "mercurial",
    -- "bazaar", "svn", and then some other rare stuff.
    } deriving (Eq, Show)

instance Sql.FromRow SourceRepository where
    fromRow = SourceRepository
        <$> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field

instance Sql.ToRow SourceRepository where
    toRow session =
        [ Sql.toField $ branch session
        , Sql.toField $ kind session
        , Sql.toField $ location session
        , Sql.toField $ module_ session
        , Sql.toField $ package session
        , Sql.toField $ subdir session
        , Sql.toField $ tag session
        , Sql.toField $ type_ session
        ]

migrations :: [Migration.Migration]
migrations =
    [ Migration.new 2021 6 19 8 33 0
        "create table sourceRepository \
        \(key integer not null primary key, \
        \branch text, \
        \kind text not null, \
        \location text, \
        \module text, \
        \package integer not null, \
        \subdir text, \
        \tag text, \
        \type text)"
    , Migration.new 2021 6 19 9 21 0
        "create index sourceRepository_package on sourceRepository (package)"
    ]

insert :: Sql.Connection -> SourceRepository -> IO Key
insert connection sourceRepository = do
    Sql.execute
        connection
        "insert into sourceRepository \
        \(branch, kind, location, module, package, subdir, tag, type) values \
        \(?, ?, ?, ?, ?, ?, ?, ?)"
        sourceRepository
    fmap (from @Int64) $ Sql.lastInsertRowId connection

deleteByPackage :: Sql.Connection -> Package.Key -> IO ()
deleteByPackage connection package = Sql.execute
    connection
    "delete from sourceRepository where package = ?"
    [package]

selectByPackage :: Sql.Connection -> Package.Key -> IO [Model]
selectByPackage connection package = Sql.query
    connection
    "select key, branch, kind, location, module, package, subdir, tag, type \
    \from sourceRepository \
    \where package = ?"
    [package]
