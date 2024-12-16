module Day14.Common (parse, Puzzle (..), Robot (..), move) where

import Utils.IO (loadInput)
import Utils.Parser (Parser, char, doParse, eol, integer, some, string, token)

data Robot = Robot {pos :: (Int, Int), speed :: (Int, Int)} deriving (Eq, Show)

data Puzzle = Puzzle {robots :: [Robot], width :: Int, height :: Int} deriving (Eq, Show)

parse :: Parser Puzzle
parse = do
  string "W="
  w <- integer
  string ", H="
  h <- integer
  eol
  robots <- some parseRobot
  return $ Puzzle robots w h

parsePair :: Char -> Parser (Int, Int)
parsePair c = do
  token c
  token '='
  first <- integer
  token ','
  second <- integer
  return (first, second)

parseRobot :: Parser Robot
parseRobot = do
  pos <- parsePair 'p'
  token ' '
  speed <- parsePair 'v'
  eol
  return $ Robot pos speed

move :: (Int, Int) -> Int -> Robot -> (Int, Int)
move (w, h) amt (Robot (x, y) (vx, vy)) = ((x + amt * (vx + w)) `mod` w, (y + amt * (vy + h)) `mod` h)
