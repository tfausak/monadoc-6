module Monadoc.Type.VersionRange where

import Monadoc.Prelude

import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Pretty as Cabal
import qualified Distribution.Types.VersionRange as Cabal

newtype VersionRange
    = VersionRange Cabal.VersionRange
    deriving (Eq, Show)

instance From VersionRange String where
    from = Cabal.prettyShow . into @Cabal.VersionRange

instance From Cabal.VersionRange VersionRange where
    from = VersionRange . Cabal.normaliseVersionRange

instance From VersionRange Cabal.VersionRange where
    from (VersionRange x) = x

instance TryFrom String VersionRange where
    tryFrom = maybeTryFrom $ fmap (from @Cabal.VersionRange) . Cabal.simpleParsec

instance Sql.FromField VersionRange where
    fromField field = do
        string <- Sql.fromField field
        case tryFrom @String string of
            Left _ -> Sql.returnError Sql.ConversionFailed field "invalid VersionRange"
            Right versionRange -> pure versionRange

instance Sql.ToField VersionRange where
    toField = Sql.toField . into @String

any :: VersionRange
any = from Cabal.anyVersion
