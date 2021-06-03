module Monadoc.Type.PackageName where

import Monadoc.Prelude

import qualified Distribution.Parsec as Cabal
import qualified Distribution.Types.PackageName as Cabal

newtype PackageName
    = PackageName Cabal.PackageName
    deriving (Eq, Ord, Show)

instance TryFrom String PackageName where
    tryFrom = maybeTryFrom <| \ s -> case Cabal.simpleParsec s of
        Nothing -> Nothing
        Just n -> Just <| from @Cabal.PackageName n

instance From PackageName String where
    from = into @Cabal.PackageName .> Cabal.unPackageName

instance From Cabal.PackageName PackageName where
    from = PackageName

instance From PackageName Cabal.PackageName where
    from (PackageName x) = x
