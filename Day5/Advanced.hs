module Day5.Advanced where

import Data.Maybe (mapMaybe)
import Day5.Common (LookupTable, Order, Rule, buildLookupTable, parse, score, sortOrder)
import Utils.IO (loadInput)

fixOrder :: LookupTable -> Order -> Maybe Order
fixOrder tbl order = if fixedOrder == order then Nothing else Just fixedOrder
  where
    fixedOrder = sortOrder tbl order

fixOrders :: ([Rule], [Order]) -> [Order]
fixOrders (rules, orders) = mapMaybe (fixOrder tbl) orders
  where
    tbl = buildLookupTable rules

main :: IO ()
main = loadInput >>= print . sum . map score . fixOrders . parse
