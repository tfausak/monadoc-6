module Monadoc.Exception.NotFound where

import qualified Control.Monad.Catch as Exception

data NotFound
    = NotFound
    deriving (Eq, Show)

instance Exception.Exception NotFound

new :: NotFound
new = NotFound
