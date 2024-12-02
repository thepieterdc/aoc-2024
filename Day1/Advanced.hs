module Day1.Advanced where

import qualified Data.Map as Map
import Day1.Common (firstLast, parseLine)
import Utils.IO (loadInput)
import Utils.Lists (frequencies)
import Utils.Parser (doParse)

findFrequencies :: ([Int], [Int]) -> [Int]
findFrequencies (left, right) = map (\x -> x * Map.findWithDefault 0 x freqs) left
  where
    freqs = Map.fromList $ frequencies right

main :: IO ()
main = loadInput >>= print . sum . findFrequencies . firstLast . map (doParse parseLine) . lines
