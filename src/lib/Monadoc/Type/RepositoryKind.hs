{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.RepositoryKind where

import Monadoc.Prelude

import qualified Monadoc.Vendor.Sql as Sql
import qualified Distribution.Pretty as Cabal
import qualified Distribution.Types.SourceRepo as Cabal
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Utility.Cabal as Cabal

newtype RepositoryKind
    = RepositoryKind Cabal.RepoKind
    deriving (Eq, Ord, Show)

instance From Cabal.RepoKind RepositoryKind

instance From RepositoryKind Cabal.RepoKind

instance TryFrom String RepositoryKind where
    tryFrom = Cabal.parsecTryFrom @Cabal.RepoKind Proxy

instance From RepositoryKind String where
    from = Cabal.prettyShow . into @Cabal.RepoKind

instance Sql.FromField RepositoryKind where
    fromField = Sql.defaultFromField @String Proxy

instance Sql.ToField RepositoryKind where
    toField = Sql.toField . into @String

instance ToXml.ToXml RepositoryKind where
    toXml = ToXml.toXml . into @String
