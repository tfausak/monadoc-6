module Monadoc.Exception.InvalidJson where

import Monadoc.Prelude

data InvalidJson
    = InvalidJson LazyByteString String
    deriving (Eq, Show)

instance Exception InvalidJson

new :: LazyByteString -> String -> InvalidJson
new = InvalidJson
