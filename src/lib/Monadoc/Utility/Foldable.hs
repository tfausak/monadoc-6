module Monadoc.Utility.Foldable where

import Monadoc.Prelude

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Ord as Ord

groupBy :: (Ord k, Foldable t) => (v -> k) -> t v -> Map.Map k (NonEmpty.NonEmpty v)
groupBy f = foldr
    (\ v -> Map.alter
        (Just . maybe (NonEmpty.singleton v) (NonEmpty.cons v))
        (f v))
    Map.empty

indexBy :: (Ord k, Foldable t) => (v -> k) -> t v -> Map.Map k v
indexBy f = foldr (\ v -> Map.insert (f v) v) Map.empty

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = maximumOn id

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
