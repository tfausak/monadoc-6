{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.PackageName where

import Monadoc.Prelude

import qualified Data.Proxy as Proxy
import qualified Distribution.Types.PackageName as Cabal
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Utility.Cabal as Cabal
import qualified Monadoc.Vendor.Sql as Sql

newtype PackageName
    = PackageName Cabal.PackageName
    deriving (Eq, Ord, Show)

instance TryFrom String PackageName where
    tryFrom = Cabal.parsecTryFrom @Cabal.PackageName Proxy.Proxy

instance From PackageName String where
    from = Cabal.unPackageName . into @Cabal.PackageName

instance From Cabal.PackageName PackageName

instance From PackageName Cabal.PackageName

instance Sql.FromField PackageName where
    fromField = Sql.defaultFromField @String Proxy.Proxy

instance Sql.ToField PackageName where
    toField = Sql.toField . into @String

instance ToXml.ToXml PackageName where
    toXml = ToXml.toXml . into @String
