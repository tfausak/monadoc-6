module Monadoc.Exception.InvalidVersion where

import Monadoc.Prelude

newtype InvalidVersion
    = InvalidVersion String
    deriving (Eq, Show)

instance Exception InvalidVersion

new :: String -> InvalidVersion
new = InvalidVersion
