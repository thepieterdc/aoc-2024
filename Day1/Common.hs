module Day1.Common (firstLast, parseLine) where

import Data.Char (digitToInt, isDigit)
import Data.List (sort)
import Utils.IO (loadInput)
import Utils.Parser (Parser, char, chars, doParse, integer, some, whitespace)
import Utils.Tuples (mapTuple)

firstLast :: [(Int, Int)] -> ([Int], [Int])
firstLast inputs = (map fst inputs, map snd inputs)

parseLine :: Parser (Int, Int)
parseLine = do x <- integer; chars 3; y <- integer; return (x, y)
