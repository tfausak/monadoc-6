module Monadoc.Prelude.Extra where

import qualified Data.ByteString.Lazy
import qualified Data.Semigroup
import qualified Data.Text.Lazy

type LazyByteString = Data.ByteString.Lazy.ByteString

type LazyText = Data.Text.Lazy.Text

cons :: a -> [a] -> [a]
cons = (:)

sappend :: Data.Semigroup.Semigroup a => a -> a -> a
sappend = (Data.Semigroup.<>)
