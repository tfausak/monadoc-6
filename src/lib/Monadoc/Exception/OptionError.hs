module Monadoc.Exception.OptionError where

import Monadoc.Prelude

newtype OptionError
    = OptionError (NonEmpty String)
    deriving (Eq, Show)

instance Exception OptionError

new :: NonEmpty String -> OptionError
new = OptionError
