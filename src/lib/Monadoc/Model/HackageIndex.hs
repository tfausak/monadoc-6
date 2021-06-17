module Monadoc.Model.HackageIndex where

import Monadoc.Prelude

import qualified Data.ByteString as ByteString
import qualified Data.Maybe as Maybe
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Exception.DuplicateHackageIndex as DuplicateHackageIndex
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.Model as Model

type Model = Model.Model HackageIndex

type Key = Key.Key HackageIndex

data HackageIndex = HackageIndex
    { contents :: ByteString
    , size :: Int
    } deriving (Eq, Show)

instance Sql.FromRow HackageIndex where
    fromRow = HackageIndex
        <$> Sql.field
        <*> Sql.field

instance Sql.ToRow HackageIndex where
    toRow hackageIndex =
        [ Sql.toField $ contents hackageIndex
        , Sql.toField $ size hackageIndex
        ]

migrations :: [Migration.Migration]
migrations =
    [ Migration.new 2021 5 29 14 45 0
        "create table hackageIndex \
        \(key integer not null primary key, \
        \contents blob not null, \
        \size integer not null)"
    ]

select :: Sql.Connection -> IO (Maybe Model)
select connection = fmap Maybe.listToMaybe
    . Sql.query_ connection
    $ into @Sql.Query "select key, contents, size from hackageIndex limit 1"

insert :: Sql.Connection -> HackageIndex -> IO Key
insert connection hackageIndex = do
    rows <- Sql.query_ connection $ into @Sql.Query "select count(*) from hackageIndex"
    when (rows /= [[0 :: Int]]) $ throwM DuplicateHackageIndex.new
    Sql.execute
        connection
        (into @Sql.Query "insert into hackageIndex (contents, size) values (?, ?)")
        hackageIndex
    fmap (from @Int64) $ Sql.lastInsertRowId connection

update :: Sql.Connection -> Key -> HackageIndex -> IO ()
update connection key hackageIndex = Sql.execute connection
    (into @Sql.Query "update hackageIndex set contents = ?, size = ? where key = ?")
    (contents hackageIndex, size hackageIndex, key)

-- The Hackage index has this many null bytes at the end.
offset :: Int
offset = 1024

fromByteString :: ByteString -> HackageIndex
fromByteString contents = HackageIndex { contents, size = ByteString.length contents }
