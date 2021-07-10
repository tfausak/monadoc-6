{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.BuildType where

import qualified Distribution.Pretty as Cabal
import qualified Distribution.Types.BuildType as Cabal
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Utility.Cabal as Cabal
import qualified Monadoc.Vendor.Sql as Sql
import qualified Witch

newtype BuildType
    = BuildType Cabal.BuildType
    deriving (Eq, Show)

instance Witch.From Cabal.BuildType BuildType

instance Witch.From BuildType Cabal.BuildType

instance Witch.TryFrom String BuildType where
    tryFrom = Cabal.parsecTryFrom @Cabal.BuildType []

instance Witch.From BuildType String where
    from = Cabal.prettyShow . Witch.into @Cabal.BuildType

instance Sql.FromField BuildType where
    fromField = Sql.defaultFromField @String []

instance Sql.ToField BuildType where
    toField = Sql.toField . Witch.into @String

instance ToXml.ToXml BuildType where
    toXml = ToXml.toXml . Witch.into @String
