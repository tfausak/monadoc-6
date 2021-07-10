module Monadoc.Exception.OptionError where

import Monadoc.Prelude

import qualified Data.List.NonEmpty as NonEmpty

newtype OptionError
    = OptionError (NonEmpty.NonEmpty String)
    deriving (Eq, Show)

instance Exception OptionError

new :: NonEmpty.NonEmpty String -> OptionError
new = OptionError
