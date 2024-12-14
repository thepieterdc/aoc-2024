module Day11.Common where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)
import Utils.IO (loadInput)

type Cache = Map (Int, Int) Int

blink :: Int -> [Int]
blink a | a == 0 = [1]
blink a | even (length (show a)) = [read first, read last]
  where
    (first, last) = splitAt (length (show a) `div` 2) (show a)
blink a = [a * 2024]

combine :: Int -> (Int, Cache) -> Int -> (Int, Cache)
combine n (score, cache) x = (score + score', cache')
  where
    (score', cache') = run' n cache x

parse :: String -> [Int]
parse = map read . words

-- run :: Int -> String -> Int
run :: Int -> String -> Int
run n inp = fst $ foldl (combine n) (0, Map.empty) $ parse inp

run' :: Int -> Cache -> Int -> (Int, Cache)
run' 0 cache x = (1, cache)
run' n cache x
  | isJust fromCache = (fromJust fromCache, cache)
  | otherwise = (dfs, cacheWithDfs)
  where
    fromCache = Map.lookup (n, x) cache
    blinked = blink x
    (dfs, cache') = foldl (combine $ n - 1) (0, cache) blinked
    cacheWithDfs = Map.insert (n, x) dfs cache'
