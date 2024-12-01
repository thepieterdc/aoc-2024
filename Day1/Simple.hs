module Day1.Simple where

import Data.Char (digitToInt, isDigit)
import Data.List (sort)
import Utils.IO (loadInput)
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

main :: IO ()
main = loadInput >>= print . sum . map (abs . uncurry subtract) . uncurry zip . mapTuple sort . firstLast . map (doParse parseLine) . lines
