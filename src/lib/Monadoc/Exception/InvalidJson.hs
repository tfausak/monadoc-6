module Monadoc.Exception.InvalidJson where

import qualified Control.Monad.Catch as Exception

newtype InvalidJson
    = InvalidJson String
    deriving (Eq, Show)

instance Exception.Exception InvalidJson where
    displayException (InvalidJson x) = "invalid JSON: " <> show x
