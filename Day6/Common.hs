module Day6.Common (parseFile, traverse, walk) where

import Data.Set (Set)
import qualified Data.Set as Set
import Utils.Grid (Coordinate, Direction, Grid, move, rotateClockwise)
import Utils.Parser (Parser, doParse, eol, some, token, (<|>))
import Prelude hiding (traverse)

data Tile = Obstacle | Guard | Free deriving (Eq, Show)

parse :: Parser (Grid Tile)
parse = do some parseRow

parseFile :: String -> ((Int, Int), Set Coordinate, Coordinate)
parseFile inp = ((maxR, maxC), obstacles, guard)
  where
    grid = doParse parse inp
    guard = head [(r, c) | r <- [0 .. length grid - 1], c <- [0 .. length (grid !! r) - 1], grid !! r !! c == Guard]
    obstacles = Set.fromList [(r, c) | r <- [0 .. length grid - 1], c <- [0 .. length (grid !! r) - 1], grid !! r !! c == Obstacle]
    maxR = length grid
    maxC = length $ head grid

parseRow :: Parser [Tile]
parseRow = do tiles <- some parseTile; eol; return tiles

parseTile :: Parser Tile
parseTile = do parseObstacle <|> parseGuard <|> parseFree
  where
    parseObstacle = do token '#'; return Obstacle
    parseGuard = do token '^'; return Guard
    parseFree = do _ <- token '.'; return Free

traverse :: (Int, Int) -> Set Coordinate -> (Coordinate, Direction) -> Set Coordinate -> Set Coordinate
traverse sz@(maxR, maxC) obs (here, dir) visited = if done then Set.insert nextTile visited' else traverse sz obs nextMove visited'
  where
    nextMove@(nextTile@(nextR, nextC), nextDir) = walk obs dir here
    done = nextR == 0 || nextR == maxR - 1 || nextC == 0 || nextC == maxC - 1
    visited' = Set.insert here visited

walk :: Set Coordinate -> Direction -> Coordinate -> (Coordinate, Direction)
walk obstacles dir (hereR, hereC) = nextMove
  where
    (nextR, nextC) = move dir (hereR, hereC)
    obstacle = Set.member (nextR, nextC) obstacles
    nextMove = if obstacle then ((hereR, hereC), rotateClockwise dir) else ((nextR, nextC), dir)
