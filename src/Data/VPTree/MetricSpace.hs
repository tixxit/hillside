module Data.VPTree.MetricSpace
    (
      MetricSpace(..)
    ) where

class MetricSpace a where
    distance :: a -> a -> Double

instance MetricSpace Int where
    distance a b = fromIntegral $ abs (a - b)
