{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.RepositoryType where

import Monadoc.Prelude

import qualified Data.Proxy as Proxy
import qualified Distribution.Pretty as Cabal
import qualified Distribution.Types.SourceRepo as Cabal
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Utility.Cabal as Cabal
import qualified Monadoc.Vendor.Sql as Sql

newtype RepositoryType
    = RepositoryType Cabal.RepoType
    deriving (Eq, Ord, Show)

instance From Cabal.RepoType RepositoryType

instance From RepositoryType Cabal.RepoType

instance TryFrom String RepositoryType where
    tryFrom = Cabal.parsecTryFrom @Cabal.RepoType Proxy.Proxy

instance From RepositoryType String where
    from = Cabal.prettyShow . into @Cabal.RepoType

instance Sql.FromField RepositoryType where
    fromField = Sql.defaultFromField @String Proxy.Proxy

instance Sql.ToField RepositoryType where
    toField = Sql.toField . into @String

instance ToXml.ToXml RepositoryType where
    toXml = ToXml.toXml . into @String
