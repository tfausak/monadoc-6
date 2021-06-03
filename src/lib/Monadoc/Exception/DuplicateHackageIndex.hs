module Monadoc.Exception.DuplicateHackageIndex where

import Monadoc.Prelude

data DuplicateHackageIndex
    = DuplicateHackageIndex
    deriving (Eq, Show)

instance Exception DuplicateHackageIndex

new :: DuplicateHackageIndex
new = DuplicateHackageIndex
