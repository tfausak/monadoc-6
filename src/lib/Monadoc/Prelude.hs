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

    -- * Data types
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
    Prelude.mapM_,
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

    -- * Operators
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

import qualified Prelude
