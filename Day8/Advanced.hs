module Day8.Advanced where

import qualified Data.Set as Set
import Day8.Common (parse, run)
import Utils.IO (loadInput)

main :: IO ()
main = loadInput >>= print . Set.size . Set.fromList . run 100 . parse
