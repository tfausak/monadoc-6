module Monadoc.Type.License where

import Monadoc.Prelude

import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Pretty as Cabal
import qualified Distribution.SPDX.License as Cabal
import qualified Monadoc.Class.ToXml as ToXml

newtype License
    = License Cabal.License
    deriving (Eq, Show)

instance From Cabal.License License

instance From License Cabal.License

instance TryFrom String License where
    tryFrom = maybeTryFrom $ fmap (from @Cabal.License) . Cabal.simpleParsec

instance From License String where
    from = Cabal.prettyShow . into @Cabal.License

instance Sql.FromField License where
    fromField field = do
        string <- Sql.fromField field
        case tryFrom @String string of
            Left _-> Sql.returnError Sql.ConversionFailed field "invalid License"
            Right license -> pure license

instance Sql.ToField License where
    toField = Sql.toField . into @String

instance ToXml.ToXml License where
    toXml = ToXml.toXml . into @String
