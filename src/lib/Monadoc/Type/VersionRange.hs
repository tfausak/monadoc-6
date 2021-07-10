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
import qualified Witch

newtype VersionRange
    = VersionRange Cabal.VersionRange
    deriving (Eq, Show)

instance Witch.From VersionRange String where
    from = Cabal.prettyShow . Witch.into @Cabal.VersionRange

instance Witch.From Cabal.VersionRange VersionRange

instance Witch.From VersionRange Cabal.VersionRange

instance Witch.TryFrom String VersionRange where
    tryFrom = Cabal.parsecTryFrom @Cabal.VersionRange Proxy.Proxy

instance Sql.FromField VersionRange where
    fromField = Sql.defaultFromField @String Proxy.Proxy

instance Sql.ToField VersionRange where
    toField = Sql.toField . Witch.into @String

instance ToXml.ToXml VersionRange where
    toXml = ToXml.toXml . Witch.into @String

any :: VersionRange
any = Witch.into @VersionRange Cabal.anyVersion

none :: VersionRange
none = Witch.into @VersionRange Cabal.noVersion

contains :: Version.Version -> VersionRange -> Bool
contains v r = Cabal.withinRange (Witch.into @Cabal.Version v) (Witch.into @Cabal.VersionRange r)

union :: VersionRange -> VersionRange -> VersionRange
union x y = Witch.into @VersionRange $ Cabal.unionVersionRanges (Witch.into @Cabal.VersionRange x) (Witch.into @Cabal.VersionRange y)

unions :: [VersionRange] -> VersionRange
unions xs = case xs of
    [] -> none
    x : ys -> foldr union x ys
