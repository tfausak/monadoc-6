module Monadoc.Exception.InvalidPackageName where

import Monadoc.Prelude

newtype InvalidPackageName
    = InvalidPackageName String
    deriving (Eq, Show)

instance Exception InvalidPackageName

new :: String -> InvalidPackageName
new = InvalidPackageName
