module Day7.Simple where

import Day7.Common (run)
import Utils.IO (loadInput)

main :: IO ()
main = loadInput >>= print . run False
