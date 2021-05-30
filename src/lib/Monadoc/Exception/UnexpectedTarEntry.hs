module Monadoc.Exception.UnexpectedTarEntry where

import Monadoc.Prelude

import qualified Codec.Archive.Tar as Tar

newtype UnexpectedTarEntry
    = UnexpectedTarEntry Tar.Entry
    deriving (Eq, Show)

instance Exception UnexpectedTarEntry

new :: Tar.Entry -> UnexpectedTarEntry
new = UnexpectedTarEntry
