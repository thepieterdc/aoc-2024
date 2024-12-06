module Day6.Advanced where

import Data.Set (Set)
import qualified Data.Set as Set
import Day6.Common (Tile (..), parse, traverse, walk)
import Utils.Grid (Coordinate, Direction (North), Grid, move, rotateClockwise)
import Utils.IO (loadInput)
import Utils.Parser (doParse)
import Prelude hiding (traverse)

addObstacle :: Grid Tile -> Coordinate -> Grid Tile
addObstacle g (r, c) = take r g ++ [take c (g !! r) ++ [Obstacle] ++ drop (c + 1) (g !! r)] ++ drop (r + 1) g

hasLoop :: Grid Tile -> (Coordinate, Direction) -> Set (Coordinate, Direction) -> Bool
hasLoop g (here, dir) visited = case (done, isLoop) of
  (True, _) -> False
  (_, True) -> True
  _ -> hasLoop g nextMove visited'
  where
    nextMove@(nextTile@(nextR, nextC), nextDir) = walk g here dir
    done = nextR == 0 || nextR == length g - 1 || nextC == 0 || nextC == length (g !! nextR) - 1
    isLoop = Set.member (here, dir) visited
    visited' = Set.insert (here, dir) visited

run :: Grid Tile -> [Coordinate]
run g = filter (\t -> hasLoop (addObstacle g t) (start, North) Set.empty) $ Set.toList (Set.delete start allTiles)
  where
    start = head [(r, c) | r <- [0 .. length g - 1], c <- [0 .. length (g !! r) - 1], g !! r !! c == Guard]
    allTiles = traverse g (start, North) Set.empty

main :: IO ()
main = loadInput >>= print . length . run . doParse parse
