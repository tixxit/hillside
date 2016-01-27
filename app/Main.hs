module Main where

import Control.Monad.Random
import System.Random

import Data.VPTree
import Data.VPTree.MetricSpace

example :: VPTree Int
example = Split 0 10 (Leaf [0, 7]) (Leaf [14])

data Point = Point Double Double deriving (Show)

instance MetricSpace Point where
    distance (Point x0 y0) (Point x1 y1) = sqrt (x0 * x1 + y0 * y1)

instance Random Point where
    random = runRand $ do
        x <- liftRand random
        y <- liftRand random
        return (Point x y)
    randomR ((Point l b), (Point r t)) = runRand $ do
        x <- liftRand $ randomR (l, r)
        y <- liftRand $ randomR (b, t)
        return (Point x y)

main :: IO ()
main = putStrLn "Hello, world!"
