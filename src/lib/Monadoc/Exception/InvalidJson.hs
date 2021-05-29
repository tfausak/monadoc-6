module Monadoc.Exception.InvalidJson where

import Monadoc.Prelude

newtype InvalidJson
    = InvalidJson String
    deriving (Eq, Show)

instance Exception InvalidJson where
    displayException = sappend "invalid JSON: " <. show <. into @String

instance From String InvalidJson

instance From InvalidJson String
