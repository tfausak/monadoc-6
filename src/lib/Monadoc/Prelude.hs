module Monadoc.Prelude (
    -- * Type classes
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
    Data.ByteString.ByteString,
    Data.Map.Map,
    Data.Proxy.Proxy(Proxy),
    Data.Set.Set,
    Data.Text.Text,
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
    Witch.TryFromException(..),

    -- * Functions
    Control.Monad.guard,
    Control.Monad.unless,
    Control.Monad.void,
    Control.Monad.when,
    Data.Foldable.foldl',
    Data.Foldable.foldr',
    Data.Foldable.for_,
    Data.Foldable.sequenceA_,
    Data.Foldable.traverse_,
    Data.Traversable.for,
    Prelude.abs,
    Prelude.all,
    Prelude.any,
    Prelude.break,
    Prelude.ceiling,
    Prelude.compare,
    Prelude.const,
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
    Prelude.id,
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
    Prelude.subtract,
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
    Witch.as,
    Witch.eitherTryFrom,
    Witch.from,
    Witch.into,
    Witch.maybeTryFrom,
    Witch.over,
    Witch.tryFrom,
    Witch.tryInto,
    Witch.unsafeFrom,
    Witch.unsafeInto,
    Witch.via,

    -- * Operators
    (Data.Function.&),
    (Prelude.-),
    (Prelude..),
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
    (Prelude.$),
) where

import Monadoc.Prelude.Orphanage ()

import qualified Control.Monad
import qualified Data.ByteString
import qualified Data.Foldable
import qualified Data.Function
import qualified Data.Map
import qualified Data.Proxy
import qualified Data.Set
import qualified Data.Text
import qualified Data.Traversable
import qualified Prelude
import qualified Witch
