module Monadoc.Type.Sha256 where

import Monadoc.Prelude

import qualified Crypto.Hash as Crypto

newtype Sha256
    = Sha256 (Crypto.Digest Crypto.SHA256)
    deriving (Eq, Show)

instance From (Crypto.Digest Crypto.SHA256) Sha256

instance From Sha256 (Crypto.Digest Crypto.SHA256)

instance From Sha256 String where
    from = show . into @(Crypto.Digest Crypto.SHA256)

hash :: ByteString -> Sha256
hash = from . Crypto.hashWith Crypto.SHA256
