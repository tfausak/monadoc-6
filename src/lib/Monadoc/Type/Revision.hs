module Monadoc.Type.Revision where

import Monadoc.Prelude

newtype Revision
    = Revision Word
    deriving (Eq, Ord, Show)

instance From Word Revision

instance From Revision Word

zero :: Revision
zero = from @Word 0

increment :: Revision -> Revision
increment = over @Word (+ 1)
