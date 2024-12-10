module Day9.Common (debug, capacity, empty, full, checksum, parse, Slot (..)) where

import Data.Char (digitToInt, intToDigit)

data Slot = Slot {maxCapacity :: Int, values :: [Int]} deriving (Show)

capacity :: Slot -> Int
capacity slot = maxCapacity slot - length (values slot)

checksum :: [Slot] -> Int
checksum slots = snd $ foldl (\acc s -> (fst acc + length (values s), snd acc + sum (zipWith (*) [fst acc .. fst acc + length (values s)] (values s)))) (0, 0) slots

debug :: [Slot] -> String
debug = concatMap (\s -> map intToDigit (values s) ++ replicate (capacity s) '.')

empty :: Slot -> Bool
empty slot = null (values slot)

full :: Slot -> Bool
full slot = maxCapacity slot == length (values slot)

parse :: String -> [Slot]
parse inp = slots
  where
    parsed = map digitToInt . head $ lines inp
    slots = zipWith (\idx cap -> Slot cap $ replicate (if even idx then cap else 0) (idx `div` 2)) [0 ..] parsed
