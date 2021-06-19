module Monadoc.Type.Model where

import Monadoc.Prelude

import qualified Monadoc.Vendor.Sql as Sql
import qualified Monadoc.Type.Key as Key

data Model a = Model
    { key :: Key.Key a
    , value :: a
    } deriving (Eq, Show)

instance Sql.FromRow a => Sql.FromRow (Model a) where
    fromRow = Model <$> Sql.field <*> Sql.fromRow

instance Sql.ToRow a => Sql.ToRow (Model a) where
    toRow model = Sql.toField (key model) : Sql.toRow (value model)
