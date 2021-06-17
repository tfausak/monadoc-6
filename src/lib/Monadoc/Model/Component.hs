module Monadoc.Model.Component where

import Monadoc.Prelude

import qualified Data.Maybe as Maybe
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Type.ComponentName as ComponentName
import qualified Monadoc.Type.ComponentTag as ComponentTag
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model

type Model = Model.Model Component

type Key = Key.Key Component

data Component = Component
    { name :: ComponentName.ComponentName
    , package :: Package.Key
    , tag :: ComponentTag.ComponentTag
    } deriving (Eq, Show)

instance Sql.FromRow Component where
    fromRow = Component
        <$> Sql.field
        <*> Sql.field
        <*> Sql.field

instance Sql.ToRow Component where
    toRow component =
        [ Sql.toField $ name component
        , Sql.toField $ package component
        , Sql.toField $ tag component
        ]

migrations :: [Migration.Migration]
migrations =
    [ Migration.new 2021 6 12 17 32 0
        "create table component \
        \(key integer not null primary key, \
        \name text not null, \
        \package integer not null, \
        \tag text not null, \
        \unique (package, tag, name))"
    , Migration.new 2021 6 12 22 21 0
        "create index component_package on component (package)"
    ]

select :: Sql.Connection -> Package.Key -> ComponentTag.ComponentTag -> ComponentName.ComponentName -> IO (Maybe Model)
select connection package tag name = fmap Maybe.listToMaybe $ Sql.query
    connection
    (into @Sql.Query "select key, name, package, tag from component where package = ? and tag = ? and name = ?")
    (package, tag, name)

selectByPackage :: Sql.Connection -> Package.Key -> IO [Model]
selectByPackage connection package = Sql.query
    connection
    (into @Sql.Query "select key, name, package, tag from component where package = ?")
    [package]

insert :: Sql.Connection -> Component -> IO Key
insert connection component = do
    Sql.execute
        connection
        (into @Sql.Query "insert into component (name, package, tag) values (?, ?, ?)")
        component
    fmap (from @Int64) $ Sql.lastInsertRowId connection
