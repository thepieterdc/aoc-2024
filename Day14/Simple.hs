module Day14.Simple where

import Day14.Common (Puzzle (..), move, parse)
import Utils.IO (loadInput)
import Utils.Parser (doParse)

classify :: (Int, Int) -> (Int, Int, Int, Int) -> (Int, Int) -> (Int, Int, Int, Int)
classify (w, h) orig@(ne, se, sw, nw) (x, y) | x == div w 2 || y == div h 2 = orig
classify (w, h) (ne, se, sw, nw) (x, y) | x < div w 2 && y < div h 2 = (ne + 1, se, sw, nw)
classify (w, h) (ne, se, sw, nw) (x, y) | x >= div w 2 && y < div h 2 = (ne, se + 1, sw, nw)
classify (w, h) (ne, se, sw, nw) (x, y) | x >= div w 2 && y >= div h 2 = (ne, se, sw + 1, nw)
classify (w, h) (ne, se, sw, nw) (x, y) | x < div w 2 && y >= div h 2 = (ne, se, sw, nw + 1)

run :: Puzzle -> Int
run (Puzzle robots w h) = q1 * q2 * q3 * q4
  where
    moved = map (move (w, h) 100) robots
    (q1, q2, q3, q4) = foldl (classify (w, h)) (0, 0, 0, 0) moved

main :: IO ()
main = loadInput >>= print . run . doParse parse
