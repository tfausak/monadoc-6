module Monadoc.Exception.InvalidPort where

import qualified Control.Monad.Catch as Exception

newtype InvalidPort
    = InvalidPort String
    deriving (Eq, Show)

instance Exception.Exception InvalidPort where
    displayException (InvalidPort x) = "invalid port: " <> show x
