module Monadoc.Exception.MissingBlob where

import Monadoc.Prelude

import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Type.Sha256 as Sha256

newtype MissingBlob
    = MissingBlob Sha256.Sha256
    deriving Show

instance Exception.Exception MissingBlob

new :: Sha256.Sha256 -> MissingBlob
new = MissingBlob
