module Monadoc.Exception.InvalidJson where

import Monadoc.Prelude

import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString.Lazy as LazyByteString

data InvalidJson = InvalidJson
    { message :: String
    , input :: LazyByteString.ByteString
    } deriving (Eq, Show)

instance Exception.Exception InvalidJson

new :: String -> LazyByteString.ByteString -> InvalidJson
new = InvalidJson
