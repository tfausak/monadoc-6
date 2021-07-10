module Monadoc.Exception.InvalidJson where

import Monadoc.Prelude

import qualified Data.ByteString.Lazy as LazyByteString

data InvalidJson = InvalidJson
    { message :: String
    , input :: LazyByteString.ByteString
    } deriving (Eq, Show)

instance Exception InvalidJson

new :: String -> LazyByteString.ByteString -> InvalidJson
new = InvalidJson
