module Monadoc.Type.PackageName where

import Monadoc.Prelude

import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Types.PackageName as Cabal
import qualified Monadoc.Class.ToXml as ToXml

newtype PackageName
    = PackageName Cabal.PackageName
    deriving (Eq, Ord, Show)

instance TryFrom String PackageName where
    tryFrom = maybeTryFrom $ \ s -> case Cabal.simpleParsec s of
        Nothing -> Nothing
        Just n -> Just $ from @Cabal.PackageName n

instance From PackageName String where
    from = Cabal.unPackageName . into @Cabal.PackageName

instance From Cabal.PackageName PackageName where
    from = PackageName

instance From PackageName Cabal.PackageName where
    from (PackageName x) = x

instance Sql.FromField PackageName where
    fromField field = do
        string <- Sql.fromField field
        case tryFrom @String string of
            Left _ -> Sql.returnError Sql.ConversionFailed field "invalid PackageName"
            Right packageName -> pure packageName

instance Sql.ToField PackageName where
    toField = Sql.toField . into @String

instance ToXml.ToXml PackageName where
    toXml = ToXml.toXml . into @String
