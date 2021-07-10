module Monadoc.Prelude.Extra where

import qualified Prelude

identity :: a -> a
identity = Prelude.id

hush :: Prelude.Either x a -> Prelude.Maybe a
hush = Prelude.either (Prelude.const Prelude.Nothing) Prelude.Just
