module Day6.Simple where

import Data.Set (Set)
import qualified Data.Set as Set
import Day6.Common (parseFile, traverse)
import Utils.Grid (Coordinate, Direction (North))
import Utils.IO (loadInput)
import Prelude hiding (traverse)

run :: ((Int, Int), Set Coordinate, Coordinate) -> Set Coordinate
run ((maxR, maxC), obstacles, start) = traverse (maxR, maxC) obstacles (start, North) Set.empty

main :: IO ()
main = loadInput >>= print . length . run . parseFile
