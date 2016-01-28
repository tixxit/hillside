module Data.VPTree
    (
      VPTree(..)
    , findNearest
    , findWithin
    , listToVPTree
    , verify
    ) where

import Data.List (sortBy, maximumBy)
import Data.Ord (Ord, compare, comparing, Ordering(LT, EQ, GT))
import Data.VPTree.MetricSpace

data VPTree a = Split a Double (VPTree a) (VPTree a) | Leaf [a]
    deriving (Show)

verify :: (MetricSpace a) => VPTree a -> Maybe [a]
verify (Leaf as) = Just as
verify (Split c t l r) = do
    l' <- verify l
    r' <- verify r
    descendants <-
        if (all ((<= t) . distance c) l') && (all ((>= t) . distance c) r')
            then Just $ l' ++ r'
            else Nothing
    return descendants

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

shouldSplit :: [a] -> Bool
shouldSplit = not . null . drop 5

listToVPTree :: (MetricSpace a) => [a] -> VPTree a
listToVPTree as =
    if shouldSplit as
        then
            let (p, (t, (l, r))) = split as
            in  Split p t (listToVPTree l) (listToVPTree r)
        else
            Leaf as

merge_ :: [(a, Double)] -> [(a, Double)] -> [(a, Double)]
merge_ ((l, x) : ls) ((r, y) : rs)
    | x <= y    = (l, x) : (merge_ ls ((r, y) : rs))
    | otherwise = (r, y) : (merge_ ((l, x) : ls) rs)
merge_ [] rs = rs
merge_ ls [] = ls

merge :: Double -> [(a, Double)] -> [(a, Double)] -> [(a, Double)]
merge t l r =
    (takeWhile isSafe l) ++ (merge_ (dropWhile isSafe l) r)
    where
        isSafe = ((< t) . snd)

-- minimum, point, tree
findNearest' :: MetricSpace a => Double -> a -> VPTree a -> [(a, Double)]
findNearest' m p (Split c t l r) =
    if d < t
        then merge' (min m (t - d)) l r
        else merge' (min m (d - t)) r l
    where
        d = distance p c
        merge' m' l' r' = merge m' (findNearest' m' p l') (findNearest' m' p r')
findNearest' _ p (Leaf as) =
    sortBy (comparing snd) $ map (pair (distance p)) as

-- Returns all points in the VPTree, in order of their distance from p.
findNearest :: (MetricSpace a) => a -> VPTree a -> [(a, Double)]
findNearest = findNearest' (1 / 0)

-- Returns all points within the provided distance of the point, in order of
-- their distance from the point.
findWithin :: (MetricSpace a) => a -> Double -> VPTree a -> [(a, Double)]
findWithin p d t = takeWhile ((<= d) . snd) $ findNearest p t

pair :: (a -> b) -> a -> (a, b)
pair f a = (a, f a)
