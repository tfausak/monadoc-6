module Monadoc.Exception.HackageIndexExists where

import Monadoc.Prelude

data HackageIndexExists
    = HackageIndexExists
    deriving (Eq, Show)

instance Exception HackageIndexExists
