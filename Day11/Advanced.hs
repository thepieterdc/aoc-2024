module Day11.Advanced where

import Day11.Common (run)
import Utils.IO (loadInput)

main :: IO ()
main = loadInput >>= print . run 75
