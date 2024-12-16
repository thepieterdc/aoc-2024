module Day14.Advanced where

import Data.List (nub)
import Day14.Common (Puzzle (..), move, parse)
import Utils.IO (loadInput)
import Utils.Parser (doParse)

isXmasTree :: [(Int, Int)] -> Bool
isXmasTree robots = length robots == length (nub robots)

run :: Int -> Puzzle -> Int
run i (Puzzle robots w h)
  | isXmasTree (map (move (w, h) i) robots) = i
  | otherwise = run (i + 1) (Puzzle robots w h)

main :: IO ()
main = loadInput >>= print . run 0 . doParse parse
