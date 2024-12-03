module Day3.Common (Instruction (..), eval, parse) where

import Utils.IO (loadInput)
import Utils.Parser (Parser, char, digit, digits, doParse, integer, some, string, token, (<|>))

data Instruction = Do | Dont | Multiply Int Int | Ignore deriving (Eq, Show)

eval :: Instruction -> Int
eval (Multiply a b) = a * b
eval _ = 0

parse :: String -> [Instruction]
parse = doParse parser
  where
    parseDo = do string "do()"; return Do
    parseDont = do string "don't()"; return Dont
    parseIgnore = do char; return Ignore
    parseNext = parseMultiply <|> parseDo <|> parseDont <|> parseIgnore
    parser = do some parseNext

parseMultiply :: Parser Instruction
parseMultiply = do
  string "mul("
  a <- digits
  token ','
  b <- digits
  token ')'
  return $ Multiply (read a) (read b)
