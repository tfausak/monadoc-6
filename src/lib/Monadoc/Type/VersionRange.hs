module Monadoc.Type.VersionRange where

import Monadoc.Prelude

import qualified Distribution.Pretty as Cabal
import qualified Distribution.Types.VersionRange as Cabal

newtype VersionRange
    = VersionRange Cabal.VersionRange
    deriving (Eq, Show)

instance From VersionRange String where
    from = into @Cabal.VersionRange .> Cabal.prettyShow

instance From Cabal.VersionRange VersionRange where
    from = VersionRange

instance From VersionRange Cabal.VersionRange where
    from (VersionRange x) = x

any :: VersionRange
any = from Cabal.anyVersion
