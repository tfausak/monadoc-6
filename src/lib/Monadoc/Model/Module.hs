module Monadoc.Model.Module where

import Monadoc.Prelude

import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.ModuleName as ModuleName
import qualified Monadoc.Vendor.Sql as Sql

type Model = Model.Model Module

type Key = Key.Key Module

data Module = Module
    { component :: Component.Key
    , name :: ModuleName.ModuleName
    } deriving (Eq, Show)

instance Sql.FromRow Module where
    fromRow = Module
        <$> Sql.field
        <*> Sql.field

instance Sql.ToRow Module where
    toRow x =
        [ Sql.toField $ component x
        , Sql.toField $ name x
        ]

migrations :: [Migration.Migration]
migrations =
    [ Migration.new 2021 7 3 13 45 0
        "create table module \
        \(key integer not null primary key, \
        \component integer not null, \
        \name string not null, \
        \unique (component, name))"
    ]

selectByComponent :: Sql.Connection -> Component.Key -> IO [Model]
selectByComponent connection component = Sql.query connection
    "select key, component, name from module where component = ?"
    [component]

upsert :: Sql.Connection -> Module -> IO ()
upsert connection = Sql.execute connection
    "insert into module (component, name) \
    \values (?, ?) \
    \on conflict (component, name) \
    \do nothing"

delete :: Sql.Connection -> Key -> IO ()
delete connection key = Sql.execute connection
    "delete from module where key = ?"
    [key]
