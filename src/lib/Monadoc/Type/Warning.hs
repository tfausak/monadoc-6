module Monadoc.Type.Warning where

import Monadoc.Prelude

data Warning
    = UnexpectedArgument String
    | UnrecognizedOption String
    deriving (Eq, Show)
