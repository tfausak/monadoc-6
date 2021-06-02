module Monadoc.Exception.PackageNameMismatch where

import Monadoc.Prelude

import qualified Monadoc.Type.PackageName as PackageName

data PackageNameMismatch
    = PackageNameMismatch PackageName.PackageName PackageName.PackageName
    deriving (Eq, Show)

instance Exception PackageNameMismatch

new :: PackageName.PackageName -> PackageName.PackageName -> PackageNameMismatch
new = PackageNameMismatch
