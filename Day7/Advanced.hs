module Day7.Advanced where

import Day7.Common (run)
import Utils.IO (loadInput)

main :: IO ()
main = loadInput >>= print . run True
