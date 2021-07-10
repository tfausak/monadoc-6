{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.ModuleName where

import qualified Data.Proxy as Proxy
import qualified Distribution.ModuleName as Cabal
import qualified Distribution.Pretty as Cabal
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Utility.Cabal as Cabal
import qualified Monadoc.Vendor.Sql as Sql
import qualified Witch

newtype ModuleName
    = ModuleName Cabal.ModuleName
    deriving (Eq, Ord, Show)

instance Witch.From Cabal.ModuleName ModuleName

instance Witch.From ModuleName Cabal.ModuleName

instance Witch.TryFrom String ModuleName where
    tryFrom = Cabal.parsecTryFrom @Cabal.ModuleName Proxy.Proxy

instance Witch.From ModuleName String where
    from = Cabal.prettyShow . Witch.into @Cabal.ModuleName

instance Sql.FromField ModuleName where
    fromField = Sql.defaultFromField @String Proxy.Proxy

instance Sql.ToField ModuleName where
    toField = Sql.toField . Witch.into @String

instance ToXml.ToXml ModuleName where
    toXml = ToXml.toXml . Witch.into @String
