{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.License where

import qualified Distribution.Pretty as Cabal
import qualified Distribution.SPDX.License as Cabal
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Utility.Cabal as Cabal
import qualified Monadoc.Vendor.Sql as Sql
import qualified Witch

newtype License
    = License Cabal.License
    deriving (Eq, Show)

instance Witch.From Cabal.License License

instance Witch.From License Cabal.License

instance Witch.TryFrom String License where
    tryFrom = Cabal.parsecTryFrom @Cabal.License []

instance Witch.From License String where
    from = Cabal.prettyShow . Witch.into @Cabal.License

instance Sql.FromField License where
    fromField = Sql.defaultFromField @String []

instance Sql.ToField License where
    toField = Sql.toField . Witch.into @String

instance ToXml.ToXml License where
    toXml = ToXml.toXml . Witch.into @String
