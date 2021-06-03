module Monadoc.Exception.Mismatch where

import Monadoc.Prelude

import qualified Data.Typeable as Typeable

data Mismatch a = Mismatch
    { expected :: a
    , actual :: a
    } deriving (Eq, Show)

instance (Show a, Typeable.Typeable a) => Exception (Mismatch a)

new :: a -> a -> Mismatch a
new = Mismatch
