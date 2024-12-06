module Day6.Simple where

import Data.Set (Set)
import qualified Data.Set as Set
import Day6.Common (Tile (..), parse, traverse)
import Utils.Grid (Coordinate, Direction (North), Grid)
import Utils.IO (loadInput)
import Utils.Parser (doParse)
import Prelude hiding (traverse)

run :: Grid Tile -> Set Coordinate
run g = traverse g (start, North) Set.empty
  where
    start = head [(r, c) | r <- [0 .. length g - 1], c <- [0 .. length (g !! r) - 1], g !! r !! c == Guard]

main :: IO ()
main = loadInput >>= print . length . run . doParse parse
