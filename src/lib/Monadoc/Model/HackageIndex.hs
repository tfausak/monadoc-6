module Monadoc.Model.HackageIndex where

import Monadoc.Prelude

import qualified Data.ByteString as ByteString
import qualified Data.Maybe as Maybe
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Exception.HackageIndexExists as HackageIndexExists
import qualified Monadoc.Type.Migration as Migration

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
        [ Sql.toField <| contents hackageIndex
        , Sql.toField <| size hackageIndex
        ]

migrations :: [Migration.Migration]
migrations =
    [ Migration.new 2021 5 29 14 45 0
        "create table hackageIndex \
        \(contents blob not null, \
        \size integer not null)"
    ]

select :: Sql.Connection -> IO (Maybe HackageIndex)
select c = fmap Maybe.listToMaybe <| Sql.query_ c <| into @Sql.Query
    "select contents, size from hackageIndex limit 1"

insert :: Sql.Connection -> HackageIndex -> IO ()
insert connection hackageIndex = do
    rows <- Sql.query_ connection <| into @Sql.Query "select count(*) from hackageIndex"
    when (rows /= [[0 :: Int]]) <| throwM HackageIndexExists.new
    Sql.execute
        connection
        (into @Sql.Query "insert into hackageIndex (contents, size) values (?, ?)")
        hackageIndex

update :: Sql.Connection -> HackageIndex -> IO ()
update c = Sql.execute c <| into @Sql.Query
    "update hackageIndex set contents = ?, size = ?"

-- The Hackage index has this many null bytes at the end.
offset :: Int
offset = 1024

fromByteString :: ByteString -> HackageIndex
fromByteString contents = HackageIndex { contents, size = ByteString.length contents }
