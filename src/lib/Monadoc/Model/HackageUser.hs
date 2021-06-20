module Monadoc.Model.HackageUser where

import Monadoc.Prelude

import qualified Data.Maybe as Maybe
import qualified Monadoc.Vendor.Sql as Sql
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.HackageId as HackageId
import qualified Monadoc.Type.HackageName as HackageName
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model

type Model = Model.Model HackageUser

type Key = Key.Key HackageUser

-- | This model represents a user on Hackage. You can find a user on Hackage at
-- @https:\/\/hackage.haskell.org\/user\/:name@.
data HackageUser = HackageUser
    { id :: HackageId.HackageId
    -- ^ Hackage exposes this ID in the package index, but it doesn't appear to
    -- use it anywhere else.
    , name :: HackageName.HackageName
    -- ^ People often use their real name in PascalCase. So for example this
    -- might be @\"JaneDoe\"@.
    } deriving (Eq, Show)

instance Sql.FromRow HackageUser where
    fromRow = HackageUser
        <$> Sql.field
        <*> Sql.field

instance Sql.ToRow HackageUser where
    toRow hackageUser =
        [ Sql.toField $ id hackageUser
        , Sql.toField $ name hackageUser
        ]

migrations :: [Migration.Migration]
migrations =
    [ Migration.new 2021 6 12 14 7 0
        "create table hackageUser \
        \(key integer not null primary key, \
        \id integer not null unique, \
        \name text not null unique)"
    ]

selectByName :: Sql.Connection -> HackageName.HackageName -> IO (Maybe Model)
selectByName connection name = fmap Maybe.listToMaybe $ Sql.query
    connection
    "select key, id, name from hackageUser where name = ?"
    [name]

insert :: Sql.Connection -> HackageUser -> IO Key
insert connection hackageUser = do
    Sql.execute
        connection
        "insert into hackageUser (id, name) values (?, ?)"
        hackageUser
    fmap (from @Int64) $ Sql.lastInsertRowId connection
