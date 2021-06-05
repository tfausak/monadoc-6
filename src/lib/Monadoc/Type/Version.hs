module Monadoc.Type.Version where

import Monadoc.Prelude

import qualified Data.Version as Version
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
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

instance From Version.Version Version where
    from = into @Version . Cabal.mkVersion . Version.versionBranch

instance From Version Version.Version where
    from = Version.makeVersion . Cabal.versionNumbers . into @Cabal.Version

instance TryFrom String Version where
    tryFrom = maybeTryFrom $ fmap (from @Cabal.Version) . Cabal.simpleParsec

instance Sql.FromField Version where
    fromField field = do
        string <- Sql.fromField field
        case tryFrom @String string of
            Left _ -> Sql.returnError Sql.ConversionFailed field "invalid Version"
            Right version -> pure version

instance Sql.ToField Version where
    toField = Sql.toField . into @String

instance ToXml.ToXml Version where
    toXml = ToXml.toXml . into @String
