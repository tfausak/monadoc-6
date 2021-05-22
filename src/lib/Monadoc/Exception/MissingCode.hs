module Monadoc.Exception.MissingCode where

import qualified Control.Monad.Catch as Exception

data MissingCode
    = MissingCode
    deriving (Eq, Show)

instance Exception.Exception MissingCode where
    displayException MissingCode = "missing OAuth code"
