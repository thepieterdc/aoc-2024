module Day13.Common (parse, Machine (..), isInt) where

import Utils.Parser (Parser, char, digits, doParse, eol, integer, some, string)

data Machine = Machine {btnA :: (Double, Double), btnB :: (Double, Double), prize :: (Double, Double)} deriving (Eq, Show)

isInt :: Double -> Bool
isInt x = floor x == ceiling x

parse :: Parser [Machine]
parse = some parseMachine

parseMachine :: Parser Machine
parseMachine = do
  a <- parseButton
  b <- parseButton
  prize <- parsePrize
  eol
  return $ Machine a b prize

parseButton :: Parser (Double, Double)
parseButton = do
  string "Button "
  char
  string ": X+"
  x <- integer
  string ", Y+"
  y <- integer
  eol
  return (fromIntegral x, fromIntegral y)

parsePrize :: Parser (Double, Double)
parsePrize = do
  string "Prize: X="
  x <- integer
  string ", Y="
  y <- integer
  eol
  return (fromIntegral x, fromIntegral y)
