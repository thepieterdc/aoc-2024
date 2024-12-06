module Day6.Advanced where

import Data.Set (Set)
import qualified Data.Set as Set
import Day6.Common (parseFile, traverse, walk)
import Utils.Grid (Coordinate, Direction (North), move, rotateClockwise)
import Utils.IO (loadInput)
import Prelude hiding (traverse)

hasLoop :: (Int, Int) -> Set Coordinate -> (Coordinate, Direction) -> Set (Coordinate, Direction) -> Bool
hasLoop sz@(maxR, maxC) obs (here, dir) visited = case (done, isLoop) of
  (True, _) -> False
  (_, True) -> True
  _ -> hasLoop sz obs nextMove visited'
  where
    nextMove@(nextTile@(nextR, nextC), nextDir) = walk obs dir here
    done = nextR == 0 || nextR == maxR - 1 || nextC == 0 || nextC == maxC - 1
    isLoop = Set.member (here, dir) visited
    visited' = Set.insert (here, dir) visited

run :: ((Int, Int), Set Coordinate, Coordinate) -> Int
run (sz, obs, start) = length $ filter (\c -> hasLoop sz (Set.insert c obs) (start, North) Set.empty) $ Set.toList (Set.delete start path)
  where
    path = traverse sz obs (start, North) Set.empty

main :: IO ()
main = loadInput >>= print . run . parseFile
