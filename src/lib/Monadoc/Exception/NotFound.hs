module Monadoc.Exception.NotFound where

import Monadoc.Prelude

data NotFound
    = NotFound
    deriving (Eq, Show)

instance Exception NotFound

new :: NotFound
new = NotFound
