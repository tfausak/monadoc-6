module Monadoc.Exception.BadHackageIndexSize where

import Monadoc.Prelude

data BadHackageIndexSize
    = BadHackageIndexSize Int (Maybe Int)
    deriving (Eq, Show)

instance Exception BadHackageIndexSize
