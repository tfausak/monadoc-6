module Monadoc.Utility.Either where

import Monadoc.Prelude

toMaybe :: Either x a -> Maybe a
toMaybe = either (const Nothing) Just
