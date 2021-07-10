module Monadoc.Exception.Mismatch where

import qualified Control.Monad.Catch as Exception
import qualified Data.Typeable as Typeable

data Mismatch a = Mismatch
    { expected :: a
    , actual :: a
    } deriving (Eq, Show)

instance (Show a, Typeable.Typeable a) => Exception.Exception (Mismatch a)

new :: a -> a -> Mismatch a
new = Mismatch
