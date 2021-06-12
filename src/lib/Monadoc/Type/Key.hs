module Monadoc.Type.Key where

import Monadoc.Prelude

import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Class.ToXml as ToXml

newtype Key
    = Key Int64
    deriving (Eq, Show)

instance From Int64 Key

instance From Key Int64

instance Sql.FromField Key where
    fromField = fmap (from @Int64) . Sql.fromField

instance Sql.ToField Key where
    toField = Sql.toField . into @Int64

instance ToXml.ToXml Key where
    toXml = ToXml.toXml . into @Int64
