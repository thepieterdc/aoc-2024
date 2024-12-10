module Day8.Common where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Utils.Analysis (slope)
import Utils.Grid (Coordinate)
import Utils.IO (loadInput)

type Antenna = (Char, Coordinate)

parse :: String -> ((Int, Int), [Antenna])
parse inp = ((length $ head ys, length ys), antennas)
  where
    ys = lines inp
    maxY = length ys
    antennas = [(s, (x, maxY - 1 - y)) | (y, xs) <- zip [0 ..] ys, (x, s) <- zip [0 ..] xs, s /= '.']

findAntinodes :: Int -> (Coordinate, Coordinate) -> [Coordinate]
findAntinodes exp (ant1@(x1, y1), ant2@(x2, y2)) = antinodes1 ++ antinodes2
  where
    xDiff = abs (x2 - x1)
    yDiff = abs (y2 - y1)
    (leftMost@(lmX, lmY), rightMost@(rmX, rmY)) = if x1 < x2 then (ant1, ant2) else (ant2, ant1)
    a = slope leftMost rightMost

    -- Upward line:
    --      antinode1 = left bottom of leftmost
    --      antinode2 = right top of rightmost
    -- Downward line:
    --      antinone1 = left top of leftmost
    --      antinode2 = right bottom of rightmost
    antinodes1 = [(lmX - i * xDiff, if a < 0 then lmY + i * yDiff else lmY - i * yDiff) | i <- [1 .. exp]]
    antinodes2 = [(rmX + i * xDiff, if a < 0 then rmY - i * yDiff else rmY + i * yDiff) | i <- [1 .. exp]]

run :: Int -> ((Int, Int), [(Char, Coordinate)]) -> [Coordinate]
run expansion ((maxX, maxY), antennas) = antinodes
  where
    -- Group the antennas by frequency.
    byFrequency = Map.elems $ Map.fromListWith (++) [(freq, [coords]) | (freq, coords) <- antennas]
    -- Get the combinations of every frequency's antennas.
    combinations = concatMap (\f -> Set.toList $ Set.fromList [(min a b, max a b) | a <- f, b <- f, a /= b]) byFrequency
    -- Compute the new antinodes of every combination.
    antinodes = concatMap (filter (\(x, y) -> 0 <= x && x < maxX && 0 <= y && y < maxY) . findAntinodes expansion) combinations
