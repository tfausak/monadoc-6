{-# LANGUAGE TypeApplications #-}

module Monadoc.Model.Component where

import Monadoc.Prelude

import qualified Data.Int as Int
import qualified Data.Maybe as Maybe
import qualified Monadoc.Vendor.Sql as Sql
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Type.ComponentId as ComponentId
import qualified Monadoc.Type.ComponentName as ComponentName
import qualified Monadoc.Type.ComponentTag as ComponentTag
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Witch

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

id :: Component -> ComponentId.ComponentId
id component = ComponentId.ComponentId
    { ComponentId.tag = tag component
    , ComponentId.name = Just $ name component
    }

select :: Sql.Connection -> Package.Key -> ComponentTag.ComponentTag -> ComponentName.ComponentName -> IO (Maybe Model)
select connection p t n = fmap Maybe.listToMaybe $ Sql.query
    connection
    "select key, name, package, tag from component where package = ? and tag = ? and name = ?"
    (p, t, n)

selectByPackage :: Sql.Connection -> Package.Key -> IO [Model]
selectByPackage connection p = Sql.query
    connection
    "select key, name, package, tag from component where package = ?"
    [p]

insert :: Sql.Connection -> Component -> IO Key
insert connection component = do
    Sql.execute
        connection
        "insert into component (name, package, tag) values (?, ?, ?)"
        component
    fmap (Witch.from @Int.Int64) $ Sql.lastInsertRowId connection
