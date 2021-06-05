module Monadoc.Type.Sha256 where

import Monadoc.Prelude

import qualified Crypto.Hash as Crypto
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Text.Read as Read

newtype Sha256
    = Sha256 (Crypto.Digest Crypto.SHA256)
    deriving (Eq, Show)

instance From (Crypto.Digest Crypto.SHA256) Sha256

instance From Sha256 (Crypto.Digest Crypto.SHA256)

instance From Sha256 String where
    from = show . into @(Crypto.Digest Crypto.SHA256)

instance TryFrom String Sha256 where
    tryFrom = maybeTryFrom $ fmap (from @(Crypto.Digest Crypto.SHA256)) . Read.readMaybe

instance Sql.FromField Sha256 where
    fromField field = do
        string <- Sql.fromField field
        case tryFrom @String string of
            Left _ -> Sql.returnError Sql.ConversionFailed field "invalid Sha256"
            Right sha256 -> pure sha256

instance Sql.ToField Sha256 where
    toField = Sql.toField . into @String

hash :: ByteString -> Sha256
hash = from . Crypto.hashWith Crypto.SHA256
