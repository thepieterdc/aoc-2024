module Day11.Simple where

import Day11.Common (run)
import Utils.IO (loadInput)

main :: IO ()
main = loadInput >>= print . run 25
