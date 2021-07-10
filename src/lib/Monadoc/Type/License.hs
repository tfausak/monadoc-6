{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.License where

import Monadoc.Prelude

import qualified Data.Proxy as Proxy
import qualified Distribution.Pretty as Cabal
import qualified Distribution.SPDX.License as Cabal
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Utility.Cabal as Cabal
import qualified Monadoc.Vendor.Sql as Sql

newtype License
    = License Cabal.License
    deriving (Eq, Show)

instance From Cabal.License License

instance From License Cabal.License

instance TryFrom String License where
    tryFrom = Cabal.parsecTryFrom @Cabal.License Proxy.Proxy

instance From License String where
    from = Cabal.prettyShow . into @Cabal.License

instance Sql.FromField License where
    fromField = Sql.defaultFromField @String Proxy.Proxy

instance Sql.ToField License where
    toField = Sql.toField . into @String

instance ToXml.ToXml License where
    toXml = ToXml.toXml . into @String
