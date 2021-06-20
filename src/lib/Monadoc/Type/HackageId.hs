module Monadoc.Type.HackageId where

import Monadoc.Prelude

import qualified Monadoc.Vendor.Sql as Sql

newtype HackageId
    = HackageId Int
    deriving (Eq, Show)

instance From Int HackageId

instance From HackageId Int

instance Sql.FromField HackageId where
    fromField = fmap (from @Int) . Sql.fromField

instance Sql.ToField HackageId where
    toField = Sql.toField . into @Int
