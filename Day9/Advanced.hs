module Day9.Advanced where

import Day9.Common (Slot (..), capacity, checksum, debug, empty, maxCapacity, parse)
import Utils.IO (loadInput)

attemptCompress :: [Slot] -> Int -> Int -> [Slot]
attemptCompress slots targetIdx currentIdx
  | targetIdx >= currentIdx = slots
  | capacity (slots !! targetIdx) >= maxCapacity (slots !! currentIdx) = moveAll currentIdx targetIdx slots
  | otherwise = attemptCompress slots (targetIdx + 1) currentIdx

compress :: [Slot] -> Int -> Int -> [Slot]
compress slots targetIdx currentIdx
  -- If the target is the same or after the current index, nothing can be compressed further.
  | targetIdx >= currentIdx = slots
  -- If the current slot is not empty and the target slot can host this, move the current slot to the target slot.
  | not (empty (slots !! currentIdx)) && capacity (slots !! targetIdx) >= maxCapacity (slots !! currentIdx) = compress moved targetIdx currentIdx
  -- If the current slot is not empty and the target slot does not have enough space, attempt to find the next slot.
  | not (empty (slots !! currentIdx)) && capacity (slots !! targetIdx) < maxCapacity (slots !! currentIdx) = compress (attemptCompress slots targetIdx currentIdx) targetIdx (currentIdx - 2)
  -- Otherwise, advance to the next slot.
  | otherwise = compress slots targetIdx (currentIdx - 2)
  where
    moved = moveAll currentIdx targetIdx slots

moveAll :: Int -> Int -> [Slot] -> [Slot]
moveAll from to slots = take to slots ++ [newToSlot] ++ drop (to + 1) (take from slots) ++ [newFromSlot] ++ drop (from + 1) slots
  where
    fromSlot = slots !! from
    toSlot = slots !! to
    vals = values fromSlot
    newFromSlot = Slot (maxCapacity fromSlot) []
    newToSlot = Slot (capacity toSlot) (values toSlot ++ vals)

-- run :: [Slot] -> Int
run slots = compress slots 1 $ length slots - 1

main :: IO ()
main = loadInput >>= print . checksum . run . parse
