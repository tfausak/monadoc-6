module Monadoc.Exception.BadHackageIndexSize where

import qualified Control.Monad.Catch as Exception

data BadHackageIndexSize = BadHackageIndexSize
    { before :: Int
    , after :: Maybe Int
    } deriving (Eq, Show)

instance Exception.Exception BadHackageIndexSize

new :: Int -> Maybe Int -> BadHackageIndexSize
new = BadHackageIndexSize
