module Day13.Advanced where

import Day13.Common (Machine (..), isInt, parse)
import Utils.IO (loadInput)
import Utils.Parser (doParse)
import Utils.Tuples (mapTuple)

solve :: Machine -> Int
solve machine
  | isInt a && isInt b = 3 * floor a + floor b
  | otherwise = 0
  where
    (ax, ay) = btnA machine
    (bx, by) = btnB machine
    (px, py) = mapTuple (+ 10000000000000) $ prize machine
    b = (py * ax - px * ay) / (by * ax - bx * ay)
    a = (px - bx * b) / ax

main :: IO ()
main = loadInput >>= print . sum . map solve . doParse parse
