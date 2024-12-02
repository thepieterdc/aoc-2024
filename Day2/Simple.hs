module Day2.Simple where

import Day2.Common (isSafe, parse)
import Utils.IO (loadInput)

main :: IO ()
main = loadInput >>= print . length . filter isSafe . map parse . lines
