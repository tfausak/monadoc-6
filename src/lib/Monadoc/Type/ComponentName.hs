module Monadoc.Type.ComponentName where

import Monadoc.Prelude

import qualified Monadoc.Vendor.Sql as Sql
import qualified Distribution.Types.PackageName as Cabal
import qualified Distribution.Types.UnqualComponentName as Cabal
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Type.PackageName as PackageName

newtype ComponentName
    = ComponentName Cabal.UnqualComponentName
    deriving (Eq, Ord, Show)

instance From Cabal.UnqualComponentName ComponentName

instance From ComponentName Cabal.UnqualComponentName

-- | Note that this /should/ use the @Parsec@ instance of
-- 'Cabal.UnqualComponentName', but it doesn't seem to work properly. See
-- <https://github.com/haskell/cabal/issues/7441>.
instance TryFrom String ComponentName where
    tryFrom = maybeTryFrom $ \ string -> do
        guard . not $ null string
        pure . from @Cabal.UnqualComponentName $ Cabal.mkUnqualComponentName string

instance From ComponentName String where
    from = Cabal.unUnqualComponentName . into @Cabal.UnqualComponentName

instance Sql.FromField ComponentName where
    fromField = Sql.defaultFromField @String Proxy

instance Sql.ToField ComponentName where
    toField = Sql.toField . into @String

instance ToXml.ToXml ComponentName where
    toXml = ToXml.toXml . into @String

-- | The 'Cabal.packageNameToUnqualComponentName' function might go away. See
-- <https://github.com/haskell/cabal/issues/5811>.
instance From PackageName.PackageName ComponentName where
    from = into @ComponentName . Cabal.packageNameToUnqualComponentName . into @Cabal.PackageName

instance From ComponentName PackageName.PackageName where
    from = into @PackageName.PackageName . Cabal.unqualComponentNameToPackageName . into @Cabal.UnqualComponentName
