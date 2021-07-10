{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monadoc.Type.HackageName where

import Monadoc.Prelude

import qualified Monadoc.Vendor.Sql as Sql

newtype HackageName
    = HackageName String
    deriving (Eq, Show)

instance From String HackageName

instance From HackageName String

instance Sql.FromField HackageName where
    fromField = fmap (from @String) . Sql.fromField

instance Sql.ToField HackageName where
    toField = Sql.toField . into @String
