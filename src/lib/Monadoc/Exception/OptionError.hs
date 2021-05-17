module Monadoc.Exception.OptionError where

import qualified Control.Monad.Catch as Exception
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty

newtype OptionError
    = OptionError (NonEmpty.NonEmpty String)
    deriving (Eq, Show)

instance Exception.Exception OptionError where
    displayException (OptionError xs) = List.dropWhileEnd Char.isSpace . mconcat $ NonEmpty.toList xs
