module Day9.Simple where

import Day9.Common (Slot (..), capacity, checksum, empty, full, parse)
import Utils.IO (loadInput)

compress :: [Slot] -> Int -> Int -> [Slot]
compress slots target currentIdx
  -- If the target is the same or after the current index, nothing can be compressed further.
  | target >= currentIdx = slots
  -- If the current slot is not empty and the target slot has space left, move the current slot to the first slot.
  | not (empty (slots !! currentIdx)) && not (full (slots !! target)) = compress moved target currentIdx
  -- If the current slot is not empty and the target slot is full, move to the next target slot.
  | not (empty (slots !! currentIdx)) && full (slots !! target) = compress slots (target + 2) currentIdx
  -- If the current slot is empty, move to the previous slot.
  | empty (slots !! currentIdx) = compress slots target (currentIdx - 2)
  | otherwise = slots
  where
    moved = move currentIdx target slots

move :: Int -> Int -> [Slot] -> [Slot]
move from to slots = take to slots ++ [newToSlot] ++ drop (to + 1) (take from slots) ++ [newFromSlot] ++ drop (from + 1) slots
  where
    fromSlot = slots !! from
    toSlot = slots !! to
    maxMovable = min (capacity toSlot - length (values toSlot)) (length (values fromSlot))
    vals = take maxMovable $ reverse $ values fromSlot
    newFromSlot = Slot (capacity fromSlot) (drop maxMovable $ values fromSlot)
    newToSlot = Slot (capacity toSlot) (values toSlot ++ vals)

run :: [Slot] -> Int
run slots = checksum $ compress slots 1 $ length slots - 1

main :: IO ()
main = loadInput >>= print . run . parse
