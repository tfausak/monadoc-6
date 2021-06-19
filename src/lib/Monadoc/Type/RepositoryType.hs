module Monadoc.Type.RepositoryType where

import Monadoc.Prelude

import qualified Monadoc.Vendor.Sql as Sql
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Pretty as Cabal
import qualified Distribution.Types.SourceRepo as Cabal
import qualified Monadoc.Class.ToXml as ToXml

newtype RepositoryType
    = RepositoryType Cabal.RepoType
    deriving (Eq, Show)

instance From Cabal.RepoType RepositoryType

instance From RepositoryType Cabal.RepoType

instance TryFrom String RepositoryType where
    tryFrom = eitherTryFrom $ bimap userError (from @Cabal.RepoType) . Cabal.eitherParsec

instance From RepositoryType String where
    from = Cabal.prettyShow . into @Cabal.RepoType

instance Sql.FromField RepositoryType where
    fromField = Sql.defaultFromField @String Proxy

instance Sql.ToField RepositoryType where
    toField = Sql.toField . into @String

instance ToXml.ToXml RepositoryType where
    toXml = ToXml.toXml . into @String
