{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.PackageName where

import qualified Data.Proxy as Proxy
import qualified Distribution.Types.PackageName as Cabal
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Utility.Cabal as Cabal
import qualified Monadoc.Vendor.Sql as Sql
import qualified Witch

newtype PackageName
    = PackageName Cabal.PackageName
    deriving (Eq, Ord, Show)

instance Witch.TryFrom String PackageName where
    tryFrom = Cabal.parsecTryFrom @Cabal.PackageName Proxy.Proxy

instance Witch.From PackageName String where
    from = Cabal.unPackageName . Witch.into @Cabal.PackageName

instance Witch.From Cabal.PackageName PackageName

instance Witch.From PackageName Cabal.PackageName

instance Sql.FromField PackageName where
    fromField = Sql.defaultFromField @String Proxy.Proxy

instance Sql.ToField PackageName where
    toField = Sql.toField . Witch.into @String

instance ToXml.ToXml PackageName where
    toXml = ToXml.toXml . Witch.into @String
