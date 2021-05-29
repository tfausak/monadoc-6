module Monadoc.Exception.OptionError where

import Monadoc.Prelude

import qualified Control.Monad.Catch as Exception
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Witch

newtype OptionError
    = OptionError (NonEmpty.NonEmpty String)
    deriving (Eq, Show)

instance Exception.Exception OptionError where
    displayException = List.dropWhileEnd Char.isSpace . mconcat . Witch.into @[String] . Witch.into @(NonEmpty.NonEmpty String)

instance Witch.From (NonEmpty.NonEmpty String) OptionError

instance Witch.From OptionError (NonEmpty.NonEmpty String)
