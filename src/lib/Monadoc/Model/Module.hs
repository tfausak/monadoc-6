module Monadoc.Model.Module where

import Monadoc.Prelude

import qualified Data.Maybe as Maybe
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Model.File as File
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
    , file :: Maybe File.Key
    } deriving (Eq, Show)

instance Sql.FromRow Module where
    fromRow = Module
        <$> Sql.field
        <*> Sql.field
        <*> Sql.field

instance Sql.ToRow Module where
    toRow x =
        [ Sql.toField $ component x
        , Sql.toField $ name x
        , Sql.toField $ file x
        ]

migrations :: [Migration.Migration]
migrations =
    [ Migration.new 2021 7 3 13 45 0
        "create table module \
        \(key integer not null primary key, \
        \component integer not null, \
        \name string not null, \
        \file integer, \
        \unique (component, name))"
    ]

select :: Sql.Connection -> Component.Key -> ModuleName.ModuleName -> IO (Maybe Model)
select connection c n = fmap Maybe.listToMaybe $ Sql.query connection
    "select key, component, name, file from module where component = ? and name = ?"
    (c, n)

selectByComponent :: Sql.Connection -> Component.Key -> IO [Model]
selectByComponent connection c = Sql.query connection
    "select key, component, name, file from module where component = ?"
    [c]

upsert :: Sql.Connection -> Module -> IO ()
upsert connection = Sql.execute connection
    "insert into module (component, name, file) \
    \values (?, ?, ?) \
    \on conflict (component, name) \
    \do update set file = excluded.file"

delete :: Sql.Connection -> Key -> IO ()
delete connection key = Sql.execute connection
    "delete from module where key = ?"
    [key]
