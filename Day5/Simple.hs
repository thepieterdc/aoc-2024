module Day5.Simple where

import Data.Map (Map)
import qualified Data.Map as Map (empty, findWithDefault, insert)
import Data.Set (Set, empty)
import qualified Data.Set as Set (empty, insert)
import Day2.Common (parse)
import Utils.IO (loadInput)
import Utils.Lists (indicesOf)
import Utils.Parser (Parser, char, digit, digits, doParse, eol, integer, some, token)

type Order = [Int]

type RuleMap = Map Int (Set Int)

data Rule = Rule {first :: Int, second :: Int} deriving (Eq, Show)

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

buildLookups :: (RuleMap, RuleMap) -> [Rule] -> (RuleMap, RuleMap)
buildLookups (befores, afters) (rule : rules) = buildLookups (befores', afters') rules
  where
    befores' = Map.insert (first rule) (Set.insert (second rule) $ Map.findWithDefault Set.empty (first rule) befores) befores
    afters' = Map.insert (second rule) (Set.insert (first rule) $ Map.findWithDefault Set.empty (second rule) befores) befores
buildLookups (befores, afters) [] = (befores, afters)

validOrders :: ([Rule], [Order]) -> [Order]
validOrders (rawRules, orders) = filter (validOrder rules) orders
  where
    rules@(beforeRules, afterRules) = buildLookups (Map.empty, Map.empty) rawRules

validOrder :: (RuleMap, RuleMap) -> Order -> Bool
validOrder rules order = all (validAtIndex rules order) [0 .. (length order - 1)]

validAtIndex :: (RuleMap, RuleMap) -> Order -> Int -> Bool
validAtIndex rules order idx = beforeValid
  where
    befores = Map.findWithDefault Set.empty (order !! idx) (fst rules)
    beforeValid = all (\b -> all (> idx) (indicesOf b order)) befores

rate :: [Order] -> Int
rate (order : rest) = score + rate rest
  where
    score = order !! ((length order - 1) `div` 2)
rate [] = 0

main :: IO ()
main = loadInput >>= print . rate . validOrders . doParse parseFile
