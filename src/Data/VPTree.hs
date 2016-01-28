{-|
Description : Vantage Point Trees
Copyright   : (c) Tom Switzer, 2016
License     : BSD
Maintainer  : thomas.switzer@gmail.com
Stability   : experimental

Vantage Point trees are a data structure for fast point location in arbitrary
metric spaces. They're mostly suitable for low-dimension problems.
-}
module Data.VPTree
    (
      VPTree(..)
    , findNearest
    , findWithin
    , fromList
    , verify
    ) where

import Data.List (sortBy, maximumBy)
import Data.Ord (Ord, compare, comparing, Ordering(LT, EQ, GT))
import Data.VPTree.MetricSpace

data VPTree a
    = Split a Double (VPTree a) (VPTree a)
    | Leaf [a]
    deriving (Show)

splitFrom :: (MetricSpace a) => a -> [a] -> (Double, ([a], [a]))
splitFrom p as =
    (snd $ head r, (map fst l, map fst r))
    where
        k = quot (length as) 2
        as' = sortBy (comparing snd) $ map (pair (distance p)) as
        (l, r) = splitAt k as'

split :: (MetricSpace a) => [a] -> (a, (Double, ([a], [a])))
split as =
    maximumBy cmp $ map (\p -> (p, splitFrom p as)) as
    where cmp (_, (x, _)) (_, (y, _)) = compare x y

-- splitM :: (MetricSpace a, Monad m) => ([a] -> m [(Double, ([a], [a]))]) -> [a] -> m (a, (Double, ([a], [a])))

-- Returns true if we should split a candidate leaf.
shouldSplit :: [a] -> Bool
shouldSplit = not . null . drop 5

-- | Construct a VPTree from a list of points.
fromList :: (MetricSpace a)
         => [a]      -- ^ list of points to add to the VPTree
         -> VPTree a -- ^ a vantage point tree of the points
fromList as =
    if shouldSplit as
        then
            let (p, (t, (l, r))) = split as
            in  Split p t (fromList l) (fromList r)
        else
            Leaf as

merge_ :: [(a, Double)] -> [(a, Double)] -> [(a, Double)]
merge_ ((l, x) : ls) ((r, y) : rs)
    | x <= y    = (l, x) : (merge_ ls ((r, y) : rs))
    | otherwise = (r, y) : (merge_ ((l, x) : ls) rs)
merge_ [] rs = rs
merge_ ls [] = ls

-- Merges 2 lists of points, sorted by their distance. The first argument
-- provides a lower bound on the minimum distance of the points in the 2nd
-- list. We use it to avoid traversing/forcing the 2nd list unless necessary.
merge :: Double -> [(a, Double)] -> [(a, Double)] -> [(a, Double)]
merge t l r =
    (takeWhile isSafe l) ++ (merge_ (dropWhile isSafe l) r)
    where
        isSafe = ((< t) . snd)

-- | Traverses all points in the VPTree in order of their distance to a query
-- point.  This version also uses a lower bound on the minimum distance that
-- can be safely returned before we must resort to merging lists.
findNearest' :: MetricSpace a
             => Double        -- ^ lower bound on the mininum distance
             -> a             -- ^ the query point
             -> VPTree a      -- ^ the tree being queried
             -> [(a, Double)] -- ^ results with their distance to the query point
findNearest' m p (Split c t l r) =
    if d < t
        then merge' (min m (t - d)) l r
        else merge' (min m (d - t)) r l
    where
        d = distance p c
        merge' m' l' r' = merge m' (findNearest' m' p l') (findNearest' m' p r')
findNearest' _ p (Leaf as) =
    sortBy (comparing snd) $ map (pair (distance p)) as

-- | Returns all points in the VPTree, in order of their distance from p.
findNearest :: MetricSpace a
            => a              -- ^ the query point
            -> VPTree a       -- ^ the tree being searched
            -> [(a, Double)]  -- ^ results along with the distance to the query point
findNearest = findNearest' (1 / 0)

-- | Returns all points within the provided distance of the point, in order of
-- their distance from the point.
findWithin :: MetricSpace a
           => a             -- ^ the query point
           -> Double        -- ^ the maximum distance to search
           -> VPTree a      -- ^ the tree being searched
           -> [a]           -- ^ the results of the search
findWithin p d t = map fst $ takeWhile ((<= d) . snd) $ findNearest p t

pair :: (a -> b) -> a -> (a, b)
pair f a = (a, f a)
