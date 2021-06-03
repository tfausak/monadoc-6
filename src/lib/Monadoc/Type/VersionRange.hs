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
    from = into @Cabal.VersionRange .> Cabal.prettyShow

instance From Cabal.VersionRange VersionRange where
    from = Cabal.normaliseVersionRange .> VersionRange

instance From VersionRange Cabal.VersionRange where
    from (VersionRange x) = x

instance TryFrom String VersionRange where
    tryFrom = maybeTryFrom <| Cabal.simpleParsec .> fmap (from @Cabal.VersionRange)

instance Sql.FromField VersionRange where
    fromField field = do
        string <- Sql.fromField field
        case tryFrom @String string of
            Left _ -> Sql.returnError Sql.ConversionFailed field "invalid VersionRange"
            Right versionRange -> pure versionRange

instance Sql.ToField VersionRange where
    toField = into @String .> Sql.toField

any :: VersionRange
any = from Cabal.anyVersion
