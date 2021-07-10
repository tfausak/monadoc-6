{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.BuildType where

import Monadoc.Prelude

import qualified Monadoc.Vendor.Sql as Sql
import qualified Distribution.Pretty as Cabal
import qualified Distribution.Types.BuildType as Cabal
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Utility.Cabal as Cabal

newtype BuildType
    = BuildType Cabal.BuildType
    deriving (Eq, Show)

instance From Cabal.BuildType BuildType

instance From BuildType Cabal.BuildType

instance TryFrom String BuildType where
    tryFrom = Cabal.parsecTryFrom @Cabal.BuildType Proxy

instance From BuildType String where
    from = Cabal.prettyShow . into @Cabal.BuildType

instance Sql.FromField BuildType where
    fromField = Sql.defaultFromField @String Proxy

instance Sql.ToField BuildType where
    toField = Sql.toField . into @String

instance ToXml.ToXml BuildType where
    toXml = ToXml.toXml . into @String
