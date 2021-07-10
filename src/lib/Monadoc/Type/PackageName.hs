{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.PackageName where

import Monadoc.Prelude

import qualified Monadoc.Vendor.Sql as Sql
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Types.PackageName as Cabal
import qualified Monadoc.Class.ToXml as ToXml

newtype PackageName
    = PackageName Cabal.PackageName
    deriving (Eq, Ord, Show)

instance TryFrom String PackageName where
    tryFrom = eitherTryFrom $ bimap userError (from @Cabal.PackageName) . Cabal.eitherParsec

instance From PackageName String where
    from = Cabal.unPackageName . into @Cabal.PackageName

instance From Cabal.PackageName PackageName

instance From PackageName Cabal.PackageName

instance Sql.FromField PackageName where
    fromField = Sql.defaultFromField @String Proxy

instance Sql.ToField PackageName where
    toField = Sql.toField . into @String

instance ToXml.ToXml PackageName where
    toXml = ToXml.toXml . into @String
