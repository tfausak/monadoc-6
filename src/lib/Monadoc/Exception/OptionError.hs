module Monadoc.Exception.OptionError where

import Monadoc.Prelude

import qualified Data.Char as Char
import qualified Data.List as List

newtype OptionError
    = OptionError (NonEmpty String)
    deriving (Eq, Show)

instance Exception OptionError where
    displayException = List.dropWhileEnd Char.isSpace <. mconcat <. into @[String] <. into @(NonEmpty String)

instance From (NonEmpty String) OptionError

instance From OptionError (NonEmpty String)
