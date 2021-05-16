module Monadoc.Exception.OptionError where

import qualified Control.Monad.Catch as Exception
import qualified Data.List.NonEmpty as NonEmpty

newtype OptionError
    = OptionError (NonEmpty.NonEmpty String)
    deriving (Eq, Show)

instance Exception.Exception OptionError where
    displayException (OptionError xs) = mconcat $ NonEmpty.toList xs
