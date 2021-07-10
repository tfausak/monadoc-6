module Monadoc.Prelude.Extra where

import qualified Prelude

always :: a -> b -> a
always = Prelude.const

identity :: a -> a
identity = Prelude.id

hush :: Prelude.Either x a -> Prelude.Maybe a
hush = Prelude.either (always Prelude.Nothing) Prelude.Just
