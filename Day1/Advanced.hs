module Day1.Advanced where

import Data.Char (digitToInt, isDigit)
import Data.List (sort)
import qualified Data.Map as Map
import Utils.IO (loadInput)
import Utils.Lists (frequencies)
import Utils.Parser (Parser, char, chars, doParse, integer, some, whitespace)
import Utils.Tuples (mapTuple)

firstLast :: [(Int, Int)] -> ([Int], [Int])
firstLast ((a, b) : rest) = (a : as, b : bs)
  where
    (as, bs) = firstLast rest
firstLast [] = ([], [])

parseLine :: Parser (Int, Int)
parseLine = do
  x <- integer
  _ <- chars 3
  y <- integer
  return (x, y)

findFrequencies :: ([Int], [Int]) -> [Int]
findFrequencies (left, right) = map (\x -> x * Map.findWithDefault 0 x freqs) left
  where
    freqs = Map.fromList $ frequencies right

main :: IO ()
main = loadInput >>= print . sum . findFrequencies . firstLast . map (doParse parseLine) . lines
