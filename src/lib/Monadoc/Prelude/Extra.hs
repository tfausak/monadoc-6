module Monadoc.Prelude.Extra where

import qualified Data.ByteString.Lazy
import qualified Data.Semigroup
import qualified Data.Text.Lazy
import qualified Prelude

type LazyByteString = Data.ByteString.Lazy.ByteString

type LazyText = Data.Text.Lazy.Text

type List = []

type Tuple = (,)

always :: a -> b -> a
always = Prelude.const

cons :: a -> List a -> List a
cons = (:)

identity :: a -> a
identity = Prelude.id

sappend :: Data.Semigroup.Semigroup a => a -> a -> a
sappend = (Data.Semigroup.<>)
