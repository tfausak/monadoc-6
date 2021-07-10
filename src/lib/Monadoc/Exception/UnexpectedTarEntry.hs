module Monadoc.Exception.UnexpectedTarEntry where

import Monadoc.Prelude

import qualified Codec.Archive.Tar as Tar
import qualified Control.Monad.Catch as Exception

newtype UnexpectedTarEntry
    = UnexpectedTarEntry Tar.Entry
    deriving (Eq, Show)

instance Exception.Exception UnexpectedTarEntry

new :: Tar.Entry -> UnexpectedTarEntry
new = UnexpectedTarEntry
