module Monadoc.Prelude (
    -- * Type classes
    Control.Monad.Catch.Exception,
    Control.Monad.Catch.MonadThrow,
    Prelude.Applicative,
    Prelude.Eq,
    Prelude.Foldable,
    Prelude.Fractional,
    Prelude.Functor,
    Prelude.Integral,
    Prelude.Monad,
    Prelude.MonadFail,
    Prelude.Monoid,
    Prelude.Num,
    Prelude.Ord,
    Prelude.Real,
    Prelude.RealFrac,
    Prelude.Semigroup,
    Prelude.Show,
    Witch.From,
    Witch.TryFrom,

    -- * Data types
    Control.Monad.Catch.SomeException(SomeException),
    Data.ByteString.ByteString,
    Data.Int.Int16,
    Data.Int.Int32,
    Data.Int.Int64,
    Data.Int.Int8,
    Data.List.NonEmpty.NonEmpty,
    Data.Map.Map,
    Data.Proxy.Proxy,
    Data.Ratio.Ratio,
    Data.Set.Set,
    Data.Text.Text,
    Data.Void.Void,
    Data.Word.Word16,
    Data.Word.Word32,
    Data.Word.Word64,
    Data.Word.Word8,
    Monadoc.Prelude.Extra.LazyByteString,
    Monadoc.Prelude.Extra.LazyText,
    Monadoc.Prelude.Extra.List,
    Monadoc.Prelude.Extra.Tuple,
    Numeric.Natural.Natural,
    Prelude.Bool(False, True),
    Prelude.Char,
    Prelude.Double,
    Prelude.Either(Left, Right),
    Prelude.FilePath,
    Prelude.Float,
    Prelude.Int,
    Prelude.Integer,
    Prelude.IO,
    Prelude.Maybe(Nothing, Just),
    Prelude.Ordering(LT, EQ, GT),
    Prelude.Rational,
    Prelude.String,
    Prelude.Word,

    -- * Functions
    Control.Monad.Catch.bracket,
    Control.Monad.Catch.catch,
    Control.Monad.Catch.displayException,
    Control.Monad.Catch.handle,
    Control.Monad.Catch.throwM,
    Control.Monad.void,
    Control.Monad.when,
    Data.Foldable.foldl',
    Data.Foldable.foldr',
    Data.Foldable.sequenceA_,
    Data.Foldable.traverse_,
    Monadoc.Prelude.Extra.always,
    Monadoc.Prelude.Extra.cons,
    Monadoc.Prelude.Extra.hush,
    Monadoc.Prelude.Extra.identity,
    Monadoc.Prelude.Extra.note,
    Monadoc.Prelude.Extra.sappend,
    Prelude.abs,
    Prelude.all,
    Prelude.any,
    Prelude.break,
    Prelude.ceiling,
    Prelude.compare,
    Prelude.curry,
    Prelude.div,
    Prelude.divMod,
    Prelude.drop,
    Prelude.dropWhile,
    Prelude.either,
    Prelude.elem,
    Prelude.error,
    Prelude.even,
    Prelude.fail,
    Prelude.filter,
    Prelude.flip,
    Prelude.floor,
    Prelude.fmap,
    Prelude.foldl,
    Prelude.foldMap,
    Prelude.foldr,
    Prelude.fst,
    Prelude.length,
    Prelude.lookup,
    Prelude.max,
    Prelude.maybe,
    Prelude.mconcat,
    Prelude.mempty,
    Prelude.min,
    Prelude.mod,
    Prelude.negate,
    Prelude.not,
    Prelude.null,
    Prelude.odd,
    Prelude.otherwise,
    Prelude.pure,
    Prelude.putStr,
    Prelude.putStrLn,
    Prelude.quot,
    Prelude.quotRem,
    Prelude.rem,
    Prelude.repeat,
    Prelude.replicate,
    Prelude.round,
    Prelude.sequenceA,
    Prelude.show,
    Prelude.snd,
    Prelude.span,
    Prelude.take,
    Prelude.takeWhile,
    Prelude.traverse,
    Prelude.truncate,
    Prelude.uncurry,
    Prelude.undefined,
    Prelude.unlines,
    Prelude.unwords,
    Prelude.unzip,
    Prelude.userError,
    Prelude.zip,
    Prelude.zipWith,
    Witch.from,
    Witch.into,
    Witch.tryFrom,
    Witch.tryInto,
    Witch.unsafeFrom,
    Witch.unsafeInto,
    Witch.via,

    -- * Operators
    (Data.Functor.$>),
    (Flow..>),
    (Flow.<.),
    (Flow.<|),
    (Flow.|>),
    (Prelude.-),
    (Prelude.*),
    (Prelude.*>),
    (Prelude./),
    (Prelude./=),
    (Prelude.&&),
    (Prelude.+),
    (Prelude.<),
    (Prelude.<*),
    (Prelude.<*>),
    (Prelude.<=),
    (Prelude.<>),
    (Prelude.<$),
    (Prelude.<$>),
    (Prelude.==),
    (Prelude.>),
    (Prelude.>=),
    (Prelude.||),
) where

import Monadoc.Prelude.Orphanage ()

import qualified Control.Monad
import qualified Control.Monad.Catch
import qualified Data.ByteString
import qualified Data.Foldable
import qualified Data.Functor
import qualified Data.Int
import qualified Data.List.NonEmpty
import qualified Data.Map
import qualified Data.Proxy
import qualified Data.Ratio
import qualified Data.Set
import qualified Data.Text
import qualified Data.Void
import qualified Data.Word
import qualified Flow
import qualified Monadoc.Prelude.Extra
import qualified Numeric.Natural
import qualified Prelude
import qualified Witch
