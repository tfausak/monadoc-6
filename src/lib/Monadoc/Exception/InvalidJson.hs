module Monadoc.Exception.InvalidJson where

import Monadoc.Prelude

import qualified Control.Monad.Catch as Exception

newtype InvalidJson
    = InvalidJson String
    deriving (Eq, Show)

instance Exception.Exception InvalidJson where
    displayException = ("invalid JSON: " <>) <<< show <<< into @String

instance From String InvalidJson

instance From InvalidJson String
