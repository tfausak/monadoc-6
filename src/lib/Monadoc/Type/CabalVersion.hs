module Monadoc.Type.CabalVersion where

import Monadoc.Prelude

import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Distribution.CabalSpecVersion as Cabal
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Type.Version as Version

newtype CabalVersion
    = CabalVersion Cabal.CabalSpecVersion
    deriving (Eq, Show)

instance From Cabal.CabalSpecVersion CabalVersion

instance From CabalVersion Cabal.CabalSpecVersion

instance TryFrom [Int] CabalVersion where
    tryFrom = maybeTryFrom $ fmap (into @CabalVersion) . Cabal.cabalSpecFromVersionDigits

instance From CabalVersion [Int] where
    from = Cabal.cabalSpecToVersionDigits . into @Cabal.CabalSpecVersion

instance TryFrom Version.Version CabalVersion where
    tryFrom = eitherTryFrom $ tryInto @CabalVersion . into @[Int]

instance From CabalVersion Version.Version where
    from = into @Version.Version . into @[Int]

instance Sql.FromField CabalVersion where
    fromField field = do
        version <- Sql.fromField field
        case tryFrom @Version.Version version of
            Left _ -> Sql.returnError Sql.ConversionFailed field "invalid CabalVersion"
            Right cabalVersion -> pure cabalVersion

instance Sql.ToField CabalVersion where
    toField = Sql.toField . into @Version.Version

instance ToXml.ToXml CabalVersion where
    toXml = ToXml.toXml . into @Version.Version
