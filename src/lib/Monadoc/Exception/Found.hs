module Monadoc.Exception.Found where

import qualified Control.Monad.Catch as Exception

newtype Found
    = Found String
    deriving (Eq, Show)

instance Exception.Exception Found

new :: String -> Found
new = Found
