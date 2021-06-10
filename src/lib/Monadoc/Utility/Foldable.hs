module Monadoc.Utility.Foldable where

import Monadoc.Prelude

import qualified Data.Ord as Ord

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = maximumOn identity

maximumOn :: (Foldable t, Ord b) => (a -> b) -> t a -> Maybe a
maximumOn = maximumBy . Ord.comparing

maximumBy :: Foldable t => (a -> a -> Ordering) -> t a -> Maybe a
maximumBy f =
    let g x = Just . maybe x (maxBy f x)
    in foldr g Nothing

maxBy :: (a -> a -> Ordering) -> a -> a -> a
maxBy f x y = case f x y of
    GT -> x
    _ -> y
