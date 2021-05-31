module Monadoc.Exception.VersionMismatch where

import Monadoc.Prelude

import qualified Distribution.Types.Version as Cabal

data VersionMismatch
    = VersionMismatch Cabal.Version Cabal.Version
    deriving (Eq, Show)

instance Exception VersionMismatch

new :: Cabal.Version -> Cabal.Version -> VersionMismatch
new = VersionMismatch
