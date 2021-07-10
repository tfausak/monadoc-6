{-# LANGUAGE FlexibleInstances #-}

module Monadoc.Type.License where

import Monadoc.Prelude

import qualified Monadoc.Vendor.Sql as Sql
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Pretty as Cabal
import qualified Distribution.SPDX.License as Cabal
import qualified Monadoc.Class.ToXml as ToXml

newtype License
    = License Cabal.License
    deriving (Eq, Show)

instance From Cabal.License License

instance From License Cabal.License

instance TryFrom String License where
    tryFrom = eitherTryFrom $ bimap userError (from @Cabal.License) . Cabal.eitherParsec

instance From License String where
    from = Cabal.prettyShow . into @Cabal.License

instance Sql.FromField License where
    fromField = Sql.defaultFromField @String Proxy

instance Sql.ToField License where
    toField = Sql.toField . into @String

instance ToXml.ToXml License where
    toXml = ToXml.toXml . into @String
