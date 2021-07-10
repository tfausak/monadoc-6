module Monadoc.Prelude.Extra where

import qualified Data.ByteString.Lazy
import qualified Data.Text.Lazy
import qualified Prelude

type LazyByteString = Data.ByteString.Lazy.ByteString

type LazyText = Data.Text.Lazy.Text

always :: a -> b -> a
always = Prelude.const

cons :: a -> [a] -> [a]
cons = (:)

identity :: a -> a
identity = Prelude.id

hush :: Prelude.Either x a -> Prelude.Maybe a
hush = Prelude.either (always Prelude.Nothing) Prelude.Just

note :: a -> Prelude.Maybe b -> Prelude.Either a b
note x = Prelude.maybe (Prelude.Left x) Prelude.Right
