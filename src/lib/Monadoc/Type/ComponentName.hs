{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.ComponentName where

import Monadoc.Prelude

import qualified Control.Monad as Monad
import qualified Data.Proxy as Proxy
import qualified Distribution.Types.PackageName as Cabal
import qualified Distribution.Types.UnqualComponentName as Cabal
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Vendor.Sql as Sql
import qualified Witch

newtype ComponentName
    = ComponentName Cabal.UnqualComponentName
    deriving (Eq, Ord, Show)

instance Witch.From Cabal.UnqualComponentName ComponentName

instance Witch.From ComponentName Cabal.UnqualComponentName

-- | Note that this /should/ use the @Parsec@ instance of
-- 'Cabal.UnqualComponentName', but it doesn't seem to work properly. See
-- <https://github.com/haskell/cabal/issues/7441>.
instance Witch.TryFrom String ComponentName where
    tryFrom = Witch.maybeTryFrom $ \ string -> do
        Monad.guard . not $ null string
        pure . Witch.from @Cabal.UnqualComponentName $ Cabal.mkUnqualComponentName string

instance Witch.From ComponentName String where
    from = Cabal.unUnqualComponentName . Witch.into @Cabal.UnqualComponentName

instance Sql.FromField ComponentName where
    fromField = Sql.defaultFromField @String Proxy.Proxy

instance Sql.ToField ComponentName where
    toField = Sql.toField . Witch.into @String

instance ToXml.ToXml ComponentName where
    toXml = ToXml.toXml . Witch.into @String

-- | The 'Cabal.packageNameToUnqualComponentName' function might go away. See
-- <https://github.com/haskell/cabal/issues/5811>.
instance Witch.From PackageName.PackageName ComponentName where
    from = Witch.into @ComponentName . Cabal.packageNameToUnqualComponentName . Witch.into @Cabal.PackageName

instance Witch.From ComponentName PackageName.PackageName where
    from = Witch.into @PackageName.PackageName . Cabal.unqualComponentNameToPackageName . Witch.into @Cabal.UnqualComponentName
