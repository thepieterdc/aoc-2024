module Day5.Simple where

import Data.Map (Map)
import qualified Data.Map as Map (empty, findWithDefault, insert)
import Data.Set (Set)
import qualified Data.Set as Set (insert)
import Day2.Common (parse)
import Utils.IO (loadInput)
import Utils.Parser (Parser, char, digit, digits, doParse, eol, integer, some, token)

type Order = [Int]

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

buildLookups :: (Map Int (Set Int), Map Int (Set Int)) -> [Rule] -> (Map Int (Set Int), Map Int (Set Int))
buildLookups (befores, afters) ((first, second) : rules) = (befores', afters)
  where
    befores' = Map.insert first (Set.insert second $ Map.findWithDefault Set.empty first befores) befores
buildLookups (befores, afters) [] = (befores, afters)

-- validOrders (rules, orders) = filter (validOrder rules) orders

-- validOrder :: [Rule] -> Order -> Bool
-- validOrder rules order = all (validAtIndex rules order) [0..length order]

-- validAtIndex :: [Rule] -> Order -> Int -> Bool
-- validAtIndex rules order idx = True
--     where

main :: IO ()
main = loadInput >>= print . buildLookups (Map.empty, Map.empty) . fst . doParse parseFile
