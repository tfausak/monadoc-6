module Monadoc.Exception.MissingHackageIndex where

import Monadoc.Prelude

data MissingHackageIndex
    = MissingHackageIndex
    deriving (Eq, Show)

instance Exception MissingHackageIndex

new :: MissingHackageIndex
new = MissingHackageIndex
