module Day5.Common (Order, LookupTable, Rule, parse, buildLookupTable, sortOrder, score) where

import Data.List (lookup, sortBy)
import Data.Map (Map)
import qualified Data.Map as Map (empty, findWithDefault, insert, lookup)
import Data.Set (Set, empty)
import qualified Data.Set as Set (empty, insert)
import Utils.IO (loadInput)
import Utils.Lists (indicesOf)
import Utils.Parser (Parser, char, digit, digits, doParse, eol, integer, some, token)

type Order = [Int]

type LookupTable = Map Int (Set Int)

data Rule = Rule {first :: Int, second :: Int} deriving (Eq, Show)

buildLookupTable :: [Rule] -> LookupTable
buildLookupTable = buildLookupTable' Map.empty
  where
    buildLookupTable' :: LookupTable -> [Rule] -> LookupTable
    buildLookupTable' tbl (rule : rules) = buildLookupTable' tbl' rules
      where
        tbl' = Map.insert (first rule) (Set.insert (second rule) $ Map.findWithDefault Set.empty (first rule) tbl) tbl
    buildLookupTable' tbl [] = tbl

parseOrder :: Parser Order
parseOrder = do nums <- some withNext; last <- digits; return $ map read $ nums ++ [last]
  where
    withNext = do num <- digits; token ','; return num

parseOrders :: Parser [Order]
parseOrders = do some parseLine
  where
    parseLine = do order <- parseOrder; eol; return order

parseRule :: Parser Rule
parseRule = do first <- digits; token '|'; Rule (read first) . read <$> digits

parseRules :: Parser [Rule]
parseRules = do some parseLine
  where
    parseLine = do rule <- parseRule; eol; return rule

parseFile :: Parser ([Rule], [Order])
parseFile = do rules <- parseRules; eol; orders <- parseOrders; return (rules, orders)

parse :: String -> ([Rule], [Order])
parse = doParse parseFile

score :: Order -> Int
score order = order !! ((length order - 1) `div` 2)

sortOrder :: LookupTable -> Order -> Order
sortOrder tbl = sortBy comparator
  where
    comparator :: Int -> Int -> Ordering
    comparator a b = case (Map.lookup a tbl, Map.lookup b tbl) of
      (Just aBefores, _) | b `elem` aBefores -> LT
      (_, Just bBefores) | a `elem` bBefores -> GT
      _ -> EQ
