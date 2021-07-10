{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.HackageName where

import qualified Monadoc.Vendor.Sql as Sql
import qualified Witch

newtype HackageName
    = HackageName String
    deriving (Eq, Show)

instance Witch.From String HackageName

instance Witch.From HackageName String

instance Sql.FromField HackageName where
    fromField = fmap (Witch.from @String) . Sql.fromField

instance Sql.ToField HackageName where
    toField = Sql.toField . Witch.into @String
