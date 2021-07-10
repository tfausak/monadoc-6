{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.Key where

import Monadoc.Prelude

import qualified Data.Int as Int
import qualified Monadoc.Vendor.Sql as Sql
import qualified Monadoc.Class.ToXml as ToXml

newtype Key a
    = Key Int.Int64
    deriving (Eq, Ord, Show)

instance From Int.Int64 (Key a)

instance From (Key a) Int.Int64

instance Sql.FromField (Key a) where
    fromField = fmap (from @Int.Int64) . Sql.fromField

instance Sql.ToField (Key a) where
    toField = Sql.toField . into @Int.Int64

instance ToXml.ToXml (Key a) where
    toXml = ToXml.toXml . into @Int.Int64
