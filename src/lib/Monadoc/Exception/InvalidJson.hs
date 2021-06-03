module Monadoc.Exception.InvalidJson where

import Monadoc.Prelude

data InvalidJson = InvalidJson
    { message :: String
    , input :: LazyByteString
    } deriving (Eq, Show)

instance Exception InvalidJson

new :: String -> LazyByteString -> InvalidJson
new = InvalidJson
