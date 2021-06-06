module Monadoc.Exception.Forbidden where

import Monadoc.Prelude

data Forbidden
    = Forbidden
    deriving (Eq, Show)

instance Exception Forbidden

new :: Forbidden
new = Forbidden
