module Monadoc.Type.Sha256 where

import Monadoc.Prelude

import qualified Crypto.Hash as Crypto
import qualified Monadoc.Vendor.Sql as Sql
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
    fromField = Sql.defaultFromField @String Proxy

instance Sql.ToField Sha256 where
    toField = Sql.toField . into @String

hash :: ByteString -> Sha256
hash = into @Sha256 . Crypto.hashWith Crypto.SHA256
