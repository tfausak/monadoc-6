{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.VersionRange where

import Monadoc.Prelude

import qualified Data.Proxy as Proxy
import qualified Distribution.Pretty as Cabal
import qualified Distribution.Types.Version as Cabal
import qualified Distribution.Types.VersionRange as Cabal
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Type.Version as Version
import qualified Monadoc.Utility.Cabal as Cabal
import qualified Monadoc.Vendor.Sql as Sql

newtype VersionRange
    = VersionRange Cabal.VersionRange
    deriving (Eq, Show)

instance From VersionRange String where
    from = Cabal.prettyShow . into @Cabal.VersionRange

instance From Cabal.VersionRange VersionRange

instance From VersionRange Cabal.VersionRange

instance TryFrom String VersionRange where
    tryFrom = Cabal.parsecTryFrom @Cabal.VersionRange Proxy.Proxy

instance Sql.FromField VersionRange where
    fromField = Sql.defaultFromField @String Proxy.Proxy

instance Sql.ToField VersionRange where
    toField = Sql.toField . into @String

instance ToXml.ToXml VersionRange where
    toXml = ToXml.toXml . into @String

any :: VersionRange
any = into @VersionRange Cabal.anyVersion

none :: VersionRange
none = into @VersionRange Cabal.noVersion

contains :: Version.Version -> VersionRange -> Bool
contains v r = Cabal.withinRange (into @Cabal.Version v) (into @Cabal.VersionRange r)

union :: VersionRange -> VersionRange -> VersionRange
union x y = into @VersionRange $ Cabal.unionVersionRanges (into @Cabal.VersionRange x) (into @Cabal.VersionRange y)

unions :: [VersionRange] -> VersionRange
unions xs = case xs of
    [] -> none
    x : ys -> foldr union x ys
