{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.RepositoryType where

import qualified Distribution.Pretty as Cabal
import qualified Distribution.Types.SourceRepo as Cabal
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Utility.Cabal as Cabal
import qualified Monadoc.Vendor.Sql as Sql
import qualified Witch

newtype RepositoryType
    = RepositoryType Cabal.RepoType
    deriving (Eq, Ord, Show)

instance Witch.From Cabal.RepoType RepositoryType

instance Witch.From RepositoryType Cabal.RepoType

instance Witch.TryFrom String RepositoryType where
    tryFrom = Cabal.parsecTryFrom @Cabal.RepoType []

instance Witch.From RepositoryType String where
    from = Cabal.prettyShow . Witch.into @Cabal.RepoType

instance Sql.FromField RepositoryType where
    fromField = Sql.defaultFromField @String []

instance Sql.ToField RepositoryType where
    toField = Sql.toField . Witch.into @String

instance ToXml.ToXml RepositoryType where
    toXml = ToXml.toXml . Witch.into @String
