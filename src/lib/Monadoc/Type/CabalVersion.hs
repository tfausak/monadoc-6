{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.CabalVersion where

import qualified Data.Proxy as Proxy
import qualified Distribution.CabalSpecVersion as Cabal
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Type.Version as Version
import qualified Monadoc.Vendor.Sql as Sql
import qualified Witch

newtype CabalVersion
    = CabalVersion Cabal.CabalSpecVersion
    deriving (Eq, Show)

instance Witch.From Cabal.CabalSpecVersion CabalVersion

instance Witch.From CabalVersion Cabal.CabalSpecVersion

instance Witch.TryFrom [Int] CabalVersion where
    tryFrom = Witch.maybeTryFrom $ fmap (Witch.into @CabalVersion) . Cabal.cabalSpecFromVersionDigits

instance Witch.From CabalVersion [Int] where
    from = Cabal.cabalSpecToVersionDigits . Witch.into @Cabal.CabalSpecVersion

instance Witch.TryFrom Version.Version CabalVersion where
    tryFrom = Witch.eitherTryFrom $ Witch.tryInto @CabalVersion . Witch.into @[Int]

instance Witch.From CabalVersion Version.Version where
    from = Witch.into @Version.Version . Witch.into @[Int]

instance Sql.FromField CabalVersion where
    fromField = Sql.defaultFromField @Version.Version Proxy.Proxy

instance Sql.ToField CabalVersion where
    toField = Sql.toField . Witch.into @Version.Version

instance ToXml.ToXml CabalVersion where
    toXml = ToXml.toXml . Witch.into @Version.Version
