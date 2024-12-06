module Day6.Common (parse, Tile (..), traverse, walk) where

import Data.Set (Set)
import qualified Data.Set as Set
import Utils.Grid (Coordinate, Direction, Grid, move, rotateClockwise)
import Utils.Parser (Parser, eol, some, token, (<|>))
import Prelude hiding (traverse)

data Tile = Obstacle | Guard | Free deriving (Eq, Show)

parse :: Parser (Grid Tile)
parse = do some parseRow

parseRow :: Parser [Tile]
parseRow = do tiles <- some parseTile; eol; return tiles

parseTile :: Parser Tile
parseTile = do parseObstacle <|> parseGuard <|> parseFree
  where
    parseObstacle = do token '#'; return Obstacle
    parseGuard = do token '^'; return Guard
    parseFree = do _ <- token '.'; return Free

traverse :: Grid Tile -> (Coordinate, Direction) -> Set Coordinate -> Set Coordinate
traverse g (here, dir) visited = if done then Set.insert nextTile visited' else traverse g nextMove visited'
  where
    nextMove@(nextTile@(nextR, nextC), nextDir) = walk g here dir
    done = nextR == 0 || nextR == length g - 1 || nextC == 0 || nextC == length (g !! nextR) - 1
    visited' = Set.insert here visited

walk :: Grid Tile -> Coordinate -> Direction -> (Coordinate, Direction)
walk g (hereR, hereC) dir = nextMove
  where
    (nextR, nextC) = move dir (hereR, hereC)
    nextTileItem = g !! nextR !! nextC
    nextMove = if nextTileItem == Obstacle then ((hereR, hereC), rotateClockwise dir) else ((nextR, nextC), dir)
