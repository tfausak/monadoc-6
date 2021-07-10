module Monadoc.Utility.Either where

toMaybe :: Either x a -> Maybe a
toMaybe = either (const Nothing) Just
