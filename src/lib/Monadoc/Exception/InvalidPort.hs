module Monadoc.Exception.InvalidPort where

import Monadoc.Prelude

newtype InvalidPort
    = InvalidPort String
    deriving (Eq, Show)

instance Exception InvalidPort

new :: String -> InvalidPort
new = InvalidPort
