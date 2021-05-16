module Monadoc.Type.Warning where

data Warning
    = UnexpectedArgument String
    | UnrecognizedOption String
    deriving (Eq, Show)
