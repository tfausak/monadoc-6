module Monadoc.Type.PackageName where

import Monadoc.Prelude

import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Types.PackageName as Cabal
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Utility.Sql as Sql

newtype PackageName
    = PackageName Cabal.PackageName
    deriving (Eq, Ord, Show)

instance TryFrom String PackageName where
    tryFrom = eitherTryFrom $ bimap userError (from @Cabal.PackageName) . Cabal.eitherParsec

instance From PackageName String where
    from = Cabal.unPackageName . into @Cabal.PackageName

instance From Cabal.PackageName PackageName where
    from = PackageName

instance From PackageName Cabal.PackageName where
    from (PackageName x) = x

instance Sql.FromField PackageName where
    fromField = Sql.defaultFromField @String Proxy

instance Sql.ToField PackageName where
    toField = Sql.toField . into @String

instance ToXml.ToXml PackageName where
    toXml = ToXml.toXml . into @String
