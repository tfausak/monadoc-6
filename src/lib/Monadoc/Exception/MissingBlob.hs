module Monadoc.Exception.MissingBlob where

import Monadoc.Prelude

import qualified Monadoc.Type.Sha256 as Sha256

newtype MissingBlob
    = MissingBlob Sha256.Sha256
    deriving Show

instance Exception MissingBlob

new :: Sha256.Sha256 -> MissingBlob
new = MissingBlob
