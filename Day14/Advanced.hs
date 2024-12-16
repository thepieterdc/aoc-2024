module Day14.Advanced where

import qualified Data.IntMap as Set
import Day14.Common (Puzzle (..), move, parse)
import Utils.IO (loadInput)
import Utils.Parser (doParse)

isXmasTree :: [(Int, Int)] -> Bool
isXmasTree robots = length robots == Set.size (Set.fromList robots)

run :: Int -> Puzzle -> Int
run i (Puzzle robots w h)
  | isXmasTree (map (move (w, h) i) robots) = i
  | otherwise = run (i + 1) (Puzzle robots w h)

main :: IO ()
main = loadInput >>= print . run 0 . doParse parse
