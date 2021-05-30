module Monadoc.Exception.InvalidPackageDescription where

import Monadoc.Prelude

newtype InvalidPackageDescription
    = InvalidPackageDescription LazyByteString
    deriving (Eq, Show)

instance Exception InvalidPackageDescription

new :: LazyByteString -> InvalidPackageDescription
new = InvalidPackageDescription
