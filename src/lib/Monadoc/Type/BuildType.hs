module Monadoc.Type.BuildType where

import Monadoc.Prelude

import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Pretty as Cabal
import qualified Distribution.Types.BuildType as Cabal
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Utility.Sql as Sql

newtype BuildType
    = BuildType Cabal.BuildType
    deriving (Eq, Show)

instance From Cabal.BuildType BuildType

instance From BuildType Cabal.BuildType

instance TryFrom String BuildType where
    tryFrom = eitherTryFrom $ bimap userError (from @Cabal.BuildType) . Cabal.eitherParsec

instance From BuildType String where
    from = Cabal.prettyShow . into @Cabal.BuildType

instance Sql.FromField BuildType where
    fromField = Sql.defaultFromField @String Proxy

instance Sql.ToField BuildType where
    toField = Sql.toField . into @String

instance ToXml.ToXml BuildType where
    toXml = ToXml.toXml . into @String
