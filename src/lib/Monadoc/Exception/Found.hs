module Monadoc.Exception.Found where

import Monadoc.Prelude

newtype Found
    = Found String
    deriving (Eq, Show)

instance Exception Found

new :: String -> Found
new = Found
