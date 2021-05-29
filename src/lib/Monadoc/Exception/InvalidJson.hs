module Monadoc.Exception.InvalidJson where

import Monadoc.Prelude

import qualified Control.Monad.Catch as Exception
import qualified Witch

newtype InvalidJson
    = InvalidJson String
    deriving (Eq, Show)

instance Exception.Exception InvalidJson where
    displayException = ("invalid JSON: " <>) . show . Witch.into @String

instance Witch.From String InvalidJson

instance Witch.From InvalidJson String
