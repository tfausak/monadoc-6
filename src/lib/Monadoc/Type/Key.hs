{-# LANGUAGE MultiParamTypeClasses #-}

module Monadoc.Type.Key where

import Monadoc.Prelude

import qualified Monadoc.Vendor.Sql as Sql
import qualified Monadoc.Class.ToXml as ToXml

newtype Key a
    = Key Int64
    deriving (Eq, Ord, Show)

instance From Int64 (Key a)

instance From (Key a) Int64

instance Sql.FromField (Key a) where
    fromField = fmap (from @Int64) . Sql.fromField

instance Sql.ToField (Key a) where
    toField = Sql.toField . into @Int64

instance ToXml.ToXml (Key a) where
    toXml = ToXml.toXml . into @Int64
