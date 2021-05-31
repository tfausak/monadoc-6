module Monadoc.Exception.PackageNameMismatch where

import Monadoc.Prelude

import qualified Distribution.Types.PackageName as Cabal

data PackageNameMismatch
    = PackageNameMismatch Cabal.PackageName Cabal.PackageName
    deriving (Eq, Show)

instance Exception PackageNameMismatch

new :: Cabal.PackageName -> Cabal.PackageName -> PackageNameMismatch
new = PackageNameMismatch
