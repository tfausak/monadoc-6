module Monadoc.Type.ComponentName where

import Monadoc.Prelude

import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Types.PackageName as Cabal
import qualified Distribution.Types.UnqualComponentName as Cabal
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Utility.Sql as Sql

newtype ComponentName
    = ComponentName Cabal.UnqualComponentName
    deriving (Eq, Ord, Show)

instance From Cabal.UnqualComponentName ComponentName

instance From ComponentName Cabal.UnqualComponentName

instance TryFrom String ComponentName where
    tryFrom = eitherTryFrom $ bimap userError (from @Cabal.UnqualComponentName) . Cabal.eitherParsec

instance From ComponentName String where
    from = Cabal.unUnqualComponentName . into @Cabal.UnqualComponentName

instance Sql.FromField ComponentName where
    fromField = Sql.defaultFromField @String Proxy

instance Sql.ToField ComponentName where
    toField = Sql.toField . into @String

instance ToXml.ToXml ComponentName where
    toXml = ToXml.toXml . into @String

instance From PackageName.PackageName ComponentName where
    from = into @ComponentName . Cabal.packageNameToUnqualComponentName . into @Cabal.PackageName

instance From ComponentName PackageName.PackageName where
    from = into @PackageName.PackageName . Cabal.unqualComponentNameToPackageName . into @Cabal.UnqualComponentName
