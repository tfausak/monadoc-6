module Monadoc.Exception.BadHackageIndexSize where

import Monadoc.Prelude

data BadHackageIndexSize = BadHackageIndexSize
    { before :: Int
    , after :: Maybe Int
    } deriving (Eq, Show)

instance Exception BadHackageIndexSize

new :: Int -> Maybe Int -> BadHackageIndexSize
new = BadHackageIndexSize
