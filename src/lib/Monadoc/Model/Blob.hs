module Monadoc.Model.Blob where

import Monadoc.Prelude

import qualified Data.ByteString as ByteString
import qualified Data.Maybe as Maybe
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Sha256 as Sha256
import qualified Monadoc.Vendor.Sql as Sql

type Model = Model.Model Blob

type Key = Key.Key Blob

data Blob = Blob
    { contents :: ByteString
    , hash :: Sha256.Sha256
    , size :: Int
    } deriving (Eq, Show)

instance Sql.FromRow Blob where
    fromRow = Blob
        <$> Sql.field
        <*> Sql.field
        <*> Sql.field

instance Sql.ToRow Blob where
    toRow blob =
        [ Sql.toField $ contents blob
        , Sql.toField $ hash blob
        , Sql.toField $ size blob
        ]

migrations :: [Migration.Migration]
migrations =
    [ Migration.new 2021 6 20 10 52 0
        "create table blob \
        \(key integer not null primary key, \
        \contents blob not null, \
        \hash text not null unique, \
        \size integer not null)"
    ]

fromByteString :: ByteString -> Blob
fromByteString c = Blob
    { contents = c
    , hash = Sha256.hash c
    , size = ByteString.length c
    }

upsert :: Sql.Connection -> Blob -> IO ()
upsert connection = Sql.execute connection
    "insert into blob (contents, hash, size) \
    \values (?, ?, ?) \
    \on conflict (hash) \
    \do nothing"

selectByHash :: Sql.Connection -> Sha256.Sha256 -> IO (Maybe Model)
selectByHash connection = fmap Maybe.listToMaybe
    . Sql.query connection "select key, contents, hash, size from blob where hash = ?"
    . Sql.Only
