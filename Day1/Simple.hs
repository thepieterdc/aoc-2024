module Day1.Simple where

import Data.List (sort)
import Day1.Common (firstLast, parseLine)
import Utils.IO (loadInput)
import Utils.Parser (doParse)
import Utils.Tuples (mapTuple)

main :: IO ()
main = loadInput >>= print . sum . map (abs . uncurry subtract) . uncurry zip . mapTuple sort . firstLast . map (doParse parseLine) . lines
