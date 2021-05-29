module Monadoc.Exception.InvalidPort where

import Monadoc.Prelude

newtype InvalidPort
    = InvalidPort String
    deriving (Eq, Show)

instance Exception InvalidPort where
    displayException = sappend "invalid port: " <. show <. into @String

instance From String InvalidPort

instance From InvalidPort String
