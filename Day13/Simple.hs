module Day13.Simple where

import Day13.Common (Machine (..), isInt, parse)
import Utils.IO (loadInput)
import Utils.Parser (doParse)

solve :: Machine -> Int
solve machine
  | 0 <= a && a <= 100 && 0 <= b && b <= 100 && isInt a && isInt b = 3 * floor a + floor b
  | otherwise = 0
  where
    (ax, ay) = btnA machine
    (bx, by) = btnB machine
    (px, py) = prize machine
    b = (py * ax - px * ay) / (by * ax - bx * ay)
    a = (px - bx * b) / ax

main :: IO ()
main = loadInput >>= print . sum . map solve . doParse parse
