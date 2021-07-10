{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.Sha256 where

import qualified Crypto.Hash as Crypto
import qualified Data.ByteString as ByteString
import qualified Data.Proxy as Proxy
import qualified Monadoc.Vendor.Sql as Sql
import qualified Text.Read as Read
import qualified Witch

newtype Sha256
    = Sha256 (Crypto.Digest Crypto.SHA256)
    deriving (Eq, Show)

instance Witch.From (Crypto.Digest Crypto.SHA256) Sha256

instance Witch.From Sha256 (Crypto.Digest Crypto.SHA256)

instance Witch.From Sha256 String where
    from = show . Witch.into @(Crypto.Digest Crypto.SHA256)

instance Witch.TryFrom String Sha256 where
    tryFrom = Witch.maybeTryFrom $ fmap (Witch.from @(Crypto.Digest Crypto.SHA256)) . Read.readMaybe

instance Sql.FromField Sha256 where
    fromField = Sql.defaultFromField @String Proxy.Proxy

instance Sql.ToField Sha256 where
    toField = Sql.toField . Witch.into @String

hash :: ByteString.ByteString -> Sha256
hash = Witch.into @Sha256 . Crypto.hashWith Crypto.SHA256
