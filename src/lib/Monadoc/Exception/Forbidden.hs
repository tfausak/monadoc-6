module Monadoc.Exception.Forbidden where

import qualified Control.Monad.Catch as Exception

data Forbidden
    = Forbidden
    deriving (Eq, Show)

instance Exception.Exception Forbidden

new :: Forbidden
new = Forbidden
