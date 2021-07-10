{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.HackageId where

import qualified Monadoc.Vendor.Sql as Sql
import qualified Witch

newtype HackageId
    = HackageId Int
    deriving (Eq, Show)

instance Witch.From Int HackageId

instance Witch.From HackageId Int

instance Sql.FromField HackageId where
    fromField = fmap (Witch.from @Int) . Sql.fromField

instance Sql.ToField HackageId where
    toField = Sql.toField . Witch.into @Int
