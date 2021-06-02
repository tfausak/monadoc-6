module Monadoc.Type.PackageName where

import Monadoc.Prelude

import qualified Distribution.Parsec as Cabal
import qualified Distribution.Types.PackageName as Cabal

newtype PackageName
    = PackageName String
    deriving (Eq, Ord, Show)

instance TryFrom String PackageName where
    tryFrom = maybeTryFrom <| Cabal.simpleParsec .> fmap (from @Cabal.PackageName)

instance From PackageName String

instance From Cabal.PackageName PackageName where
    from = Cabal.unPackageName .> unsafeInto @PackageName

instance From PackageName Cabal.PackageName where
    from = into @String .> Cabal.mkPackageName
