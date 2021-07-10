module Monadoc.Exception.OptionError where

import Monadoc.Prelude

import qualified Control.Monad.Catch as Exception
import qualified Data.List.NonEmpty as NonEmpty

newtype OptionError
    = OptionError (NonEmpty.NonEmpty String)
    deriving (Eq, Show)

instance Exception.Exception OptionError

new :: NonEmpty.NonEmpty String -> OptionError
new = OptionError
