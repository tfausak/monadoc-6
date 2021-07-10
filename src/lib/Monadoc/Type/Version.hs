{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.Version where

import qualified Data.Version as Version
import qualified Monadoc.Vendor.Sql as Sql
import qualified Distribution.Pretty as Cabal
import qualified Distribution.Types.Version as Cabal
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Utility.Cabal as Cabal
import qualified Witch

newtype Version
    = Version Cabal.Version
    deriving (Eq, Ord, Show)

instance Witch.From Version String where
    from = Cabal.prettyShow . Witch.into @Cabal.Version

instance Witch.From Cabal.Version Version

instance Witch.From Version Cabal.Version

instance Witch.From [Int] Version where
    from = Witch.into @Version . Cabal.mkVersion

instance Witch.From Version [Int] where
    from = Cabal.versionNumbers . Witch.into @Cabal.Version

instance Witch.From Version.Version Version where
    from = Witch.into @Version . Version.versionBranch

instance Witch.From Version Version.Version where
    from = Version.makeVersion . Witch.into @[Int]

instance Witch.TryFrom String Version where
    tryFrom = Cabal.parsecTryFrom @Cabal.Version []

instance Sql.FromField Version where
    fromField = Sql.defaultFromField @String []

instance Sql.ToField Version where
    toField = Sql.toField . Witch.into @String

instance ToXml.ToXml Version where
    toXml = ToXml.toXml . Witch.into @String
