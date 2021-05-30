module Monadoc.Exception.InvalidPackageVersionConstraint where

import Monadoc.Prelude

newtype InvalidPackageVersionConstraint
    = InvalidPackageVersionConstraint LazyByteString
    deriving (Eq, Show)

instance Exception InvalidPackageVersionConstraint

new :: LazyByteString -> InvalidPackageVersionConstraint
new = InvalidPackageVersionConstraint
