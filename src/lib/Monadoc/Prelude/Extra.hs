module Monadoc.Prelude.Extra where

import qualified Prelude

hush :: Prelude.Either x a -> Prelude.Maybe a
hush = Prelude.either (Prelude.const Prelude.Nothing) Prelude.Just
