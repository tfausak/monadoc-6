module Monadoc.Type.Version where

import Monadoc.Prelude

import qualified Data.Version as Version
import qualified Monadoc.Vendor.Sql as Sql
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Pretty as Cabal
import qualified Distribution.Types.Version as Cabal
import qualified Monadoc.Class.ToXml as ToXml

newtype Version
    = Version Cabal.Version
    deriving (Eq, Ord, Show)

instance From Version String where
    from = Cabal.prettyShow . into @Cabal.Version

instance From Cabal.Version Version

instance From Version Cabal.Version

instance From [Int] Version where
    from = into @Version . Cabal.mkVersion

instance From Version [Int] where
    from = Cabal.versionNumbers . into @Cabal.Version

instance From Version.Version Version where
    from = into @Version . Version.versionBranch

instance From Version Version.Version where
    from = Version.makeVersion . into @[Int]

instance TryFrom String Version where
    tryFrom = eitherTryFrom $ bimap userError (from @Cabal.Version) . Cabal.eitherParsec

instance Sql.FromField Version where
    fromField = Sql.defaultFromField @String Proxy

instance Sql.ToField Version where
    toField = Sql.toField . into @String

instance ToXml.ToXml Version where
    toXml = ToXml.toXml . into @String
