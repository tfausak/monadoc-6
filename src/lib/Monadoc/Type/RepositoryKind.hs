{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.RepositoryKind where

import qualified Data.Proxy as Proxy
import qualified Distribution.Pretty as Cabal
import qualified Distribution.Types.SourceRepo as Cabal
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Utility.Cabal as Cabal
import qualified Monadoc.Vendor.Sql as Sql
import qualified Witch

newtype RepositoryKind
    = RepositoryKind Cabal.RepoKind
    deriving (Eq, Ord, Show)

instance Witch.From Cabal.RepoKind RepositoryKind

instance Witch.From RepositoryKind Cabal.RepoKind

instance Witch.TryFrom String RepositoryKind where
    tryFrom = Cabal.parsecTryFrom @Cabal.RepoKind Proxy.Proxy

instance Witch.From RepositoryKind String where
    from = Cabal.prettyShow . Witch.into @Cabal.RepoKind

instance Sql.FromField RepositoryKind where
    fromField = Sql.defaultFromField @String Proxy.Proxy

instance Sql.ToField RepositoryKind where
    toField = Sql.toField . Witch.into @String

instance ToXml.ToXml RepositoryKind where
    toXml = ToXml.toXml . Witch.into @String
