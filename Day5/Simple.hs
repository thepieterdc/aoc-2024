module Day5.Simple where

import Day5.Common (Order, Rule, buildLookupTable, parse, score, sortOrder)
import Utils.IO (loadInput)

validOrders :: ([Rule], [Order]) -> [Order]
validOrders (rules, orders) = filter (\o -> sortOrder tbl o == o) orders
  where
    tbl = buildLookupTable rules

main :: IO ()
main = loadInput >>= print . sum . map score . validOrders . parse
