module Monadoc.Prelude
    ( Prelude.Applicative
    , Prelude.Bool(False, True)
    , Data.ByteString.ByteString
    , Prelude.Either(Left, Right)
    , Prelude.Eq
    , Prelude.FilePath
    , Prelude.Foldable
    , Witch.From
    , Prelude.Functor
    , Prelude.Int
    , Prelude.IO
    , Monadoc.Prelude.Extra.LazyByteString
    , Prelude.Maybe(Nothing, Just)
    , Prelude.Monad
    , Prelude.Monoid
    , Prelude.Semigroup
    , Prelude.Show
    , Prelude.String
    , Data.Text.Text
    , Witch.TryFrom
    , Prelude.break
    , Prelude.either
    , Prelude.flip
    , Prelude.fmap
    , Prelude.foldMap
    , Witch.from
    , Prelude.fst
    , Witch.into
    , Prelude.lookup
    , Prelude.maybe
    , Prelude.mempty
    , Prelude.mconcat
    , Prelude.pure
    , Prelude.putStr
    , Prelude.putStrLn
    , Prelude.show
    , Prelude.snd
    , Data.Foldable.traverse_
    , Prelude.unlines
    , Witch.unsafeFrom
    , Witch.unsafeInto
    , Prelude.unwords
    , Witch.via
    , (Prelude.-)
    , (Prelude..)
    , (Prelude./=)
    , (Prelude.<*>)
    , (Prelude.<>)
    , (Prelude.<$>)
    , (Prelude.==)
    , (Prelude.$)
    ) where

import qualified Data.ByteString
import qualified Data.Foldable
import qualified Data.Text
import qualified Monadoc.Prelude.Extra
import Monadoc.Prelude.Orphanage ()
import qualified Prelude
import qualified Witch
