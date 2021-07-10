module Monadoc.Exception.DuplicateHackageIndex where

import Monadoc.Prelude

import qualified Control.Monad.Catch as Exception

data DuplicateHackageIndex
    = DuplicateHackageIndex
    deriving (Eq, Show)

instance Exception.Exception DuplicateHackageIndex

new :: DuplicateHackageIndex
new = DuplicateHackageIndex
