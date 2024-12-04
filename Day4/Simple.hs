module Day4.Simple where

import Data.Maybe (catMaybes)
import Utils.IO (loadInput)

data Match = N | NE | E | SE | S | SW | W | NW deriving (Eq, Show)

type Grid = [String]

findMas :: Int -> Int -> Grid -> Int
findMas xr xc g = length $ filter (\x -> map char x == "MAS") [findN, findNE, findE, findSE, findS, findSW, findW, findNW]
  where
    char (r', c') = if r' >= 0 && r' < length g && c' >= 0 && c' < length (g !! r') then g !! r' !! c' else '.'
    findN =
      [ (r, c)
        | (r, c) <- [(xr, xc - 1), (xr, xc - 2), (xr, xc - 3)]
      ]
    findNE =
      [ (r, c)
        | (r, c) <- [(xr - 1, xc + 1), (xr - 2, xc + 2), (xr - 3, xc + 3)]
      ]
    findE =
      [ (r, c)
        | (r, c) <- [(xr, xc + 1), (xr, xc + 2), (xr, xc + 3)]
      ]
    findSE =
      [ (r, c)
        | (r, c) <- [(xr + 1, xc + 1), (xr + 2, xc + 2), (xr + 3, xc + 3)]
      ]
    findS =
      [ (r, c)
        | (r, c) <- [(xr + 1, xc), (xr + 2, xc), (xr + 3, xc)]
      ]
    findSW =
      [ (r, c)
        | (r, c) <- [(xr + 1, xc - 1), (xr + 2, xc - 2), (xr + 3, xc - 3)]
      ]
    findW =
      [ (r, c)
        | (r, c) <- [(xr, xc - 1), (xr, xc - 2), (xr, xc - 3)]
      ]
    findNW =
      [ (r, c)
        | (r, c) <- [(xr - 1, xc - 1), (xr - 2, xc - 2), (xr - 3, xc - 3)]
      ]

findXmas :: Grid -> Int
findXmas g = sum [findMas r c g | r <- [0 .. length g - 1], c <- [0 .. length (g !! r) - 1], g !! r !! c == 'X']

main :: IO ()
main = loadInput >>= print . findXmas . lines
