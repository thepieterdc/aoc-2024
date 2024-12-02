module Day2.Common (isSafe, parse) where

import Data.List (sort, sortBy)
import Data.Ord (Down (..), comparing)

isSafe :: [Int] -> Bool
isSafe l = (isAscending l || isDescending l) && absGaps3 l
  where
    isAscending l = sort l == l
    isDescending l = sortBy (comparing Data.Ord.Down) l == l
    absGaps3 l = all ((\diff -> diff > 0 && diff <= 3) . abs) (zipWith (-) l $ tail l)

parse :: String -> [Int]
parse line = map read $ words line
