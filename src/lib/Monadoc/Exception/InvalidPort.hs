module Monadoc.Exception.InvalidPort where

import Monadoc.Prelude

import qualified Control.Monad.Catch as Exception
import qualified Witch

newtype InvalidPort
    = InvalidPort String
    deriving (Eq, Show)

instance Exception.Exception InvalidPort where
    displayException = ("invalid port: " <>) . show . Witch.into @String

instance Witch.From String InvalidPort

instance Witch.From InvalidPort String
