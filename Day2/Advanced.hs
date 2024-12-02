module Day2.Advanced where

import Day2.Common (isSafe, parse)
import Utils.IO (loadInput)

isSafe' :: Int -> [Int] -> Bool
isSafe' i items = isSafeWithout i items || (i < length items) && isSafe' (i + 1) items
  where
    isSafeWithout i items = isSafe (take i items ++ drop (i + 1) items)

main :: IO ()
main = loadInput >>= print . length . filter (isSafe' 0) . map parse . lines
