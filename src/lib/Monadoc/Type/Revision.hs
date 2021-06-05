module Monadoc.Type.Revision where

import Monadoc.Prelude

import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Class.ToXml as ToXml

newtype Revision
    = Revision Word
    deriving (Eq, Ord, Show)

instance From Word Revision

instance From Revision Word

instance Sql.FromField Revision where
    fromField = fmap (from @Word) . Sql.fromField

instance Sql.ToField Revision where
    toField = Sql.toField . into @Word

instance ToXml.ToXml Revision where
    toXml = ToXml.toXml . into @Word

zero :: Revision
zero = from @Word 0

increment :: Revision -> Revision
increment = over @Word (+ 1)
