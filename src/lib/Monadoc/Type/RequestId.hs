{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.RequestId where

import Monadoc.Prelude

import qualified Data.Vault.Lazy as Vault
import qualified Network.Wai as Wai
import qualified System.IO.Unsafe as Unsafe
import qualified System.Random as Random

newtype RequestId
    = RequestId Word16
    deriving (Eq, Show)

instance From Word16 RequestId

instance From RequestId Word16

random :: IO RequestId
random = from @Word16 <$> Random.randomIO

key :: Vault.Key RequestId
key = Unsafe.unsafePerformIO Vault.newKey
{-# NOINLINE key #-}

get :: Wai.Request -> Maybe RequestId
get = Vault.lookup key . Wai.vault

set :: RequestId -> Wai.Request -> Wai.Request
set requestId request = request { Wai.vault = Vault.insert key requestId $ Wai.vault request }
