module Monadoc.Exception.InvalidPort where

import Monadoc.Prelude

import qualified Control.Monad.Catch as Exception

newtype InvalidPort
    = InvalidPort String
    deriving (Eq, Show)

instance Exception.Exception InvalidPort where
    displayException = ("invalid port: " <>) <<< show <<< into @String

instance From String InvalidPort

instance From InvalidPort String
