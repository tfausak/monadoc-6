{-# LANGUAGE FlexibleInstances #-}

module Monadoc.Type.ModuleName where

import Monadoc.Prelude

import qualified Distribution.Parsec as Cabal
import qualified Distribution.Pretty as Cabal
import qualified Distribution.ModuleName as Cabal
import qualified Monadoc.Vendor.Sql as Sql
import qualified Monadoc.Class.ToXml as ToXml

newtype ModuleName
    = ModuleName Cabal.ModuleName
    deriving (Eq, Ord, Show)

instance From Cabal.ModuleName ModuleName

instance From ModuleName Cabal.ModuleName

instance TryFrom String ModuleName where
    tryFrom = eitherTryFrom $ bimap userError (from @Cabal.ModuleName) . Cabal.eitherParsec

instance From ModuleName String where
    from = Cabal.prettyShow . into @Cabal.ModuleName

instance Sql.FromField ModuleName where
    fromField = Sql.defaultFromField @String Proxy

instance Sql.ToField ModuleName where
    toField = Sql.toField . into @String

instance ToXml.ToXml ModuleName where
    toXml = ToXml.toXml . into @String
