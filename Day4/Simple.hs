module Day4.Simple where

import Data.Maybe (catMaybes)
import Utils.IO (loadInput)

data Match = N | NE | E | SE | S | SW | W | NW deriving (Eq, Show)

type Grid = [String]

findMas :: Int -> Int -> Grid -> Int
findMas xr xc g = length $ catMaybes [findN, findNE, findE, findSE, findS, findSW, findW, findNW]
  where
    char r' c' = if r' >= 0 && r' < length g && c' >= 0 && c' < length (g !! r') then g !! r' !! c' else '.'
    findN =
      if "MAS"
        == [ char r c
             | (r, c) <- [(xr, xc - 1), (xr, xc - 2), (xr, xc - 3)]
           ]
        then Just N
        else Nothing
    findNE =
      if "MAS"
        == [ char r c
             | (r, c) <- [(xr - 1, xc + 1), (xr - 2, xc + 2), (xr - 3, xc + 3)]
           ]
        then Just NE
        else Nothing
    findE =
      if "MAS"
        == [ char r c
             | (r, c) <- [(xr, xc + 1), (xr, xc + 2), (xr, xc + 3)]
           ]
        then Just E
        else Nothing
    findSE =
      if "MAS"
        == [ char r c
             | (r, c) <- [(xr + 1, xc + 1), (xr + 2, xc + 2), (xr + 3, xc + 3)]
           ]
        then Just SE
        else Nothing
    findS =
      if "MAS"
        == [ char r c
             | (r, c) <- [(xr + 1, xc), (xr + 2, xc), (xr + 3, xc)]
           ]
        then Just S
        else Nothing
    findSW =
      if "MAS"
        == [ char r c
             | (r, c) <- [(xr + 1, xc - 1), (xr + 2, xc - 2), (xr + 3, xc - 3)]
           ]
        then Just SW
        else Nothing
    findW =
      if "MAS"
        == [ char r c
             | (r, c) <- [(xr, xc - 1), (xr, xc - 2), (xr, xc - 3)]
           ]
        then Just W
        else Nothing
    findNW =
      if "MAS"
        == [ char r c
             | (r, c) <- [(xr - 1, xc - 1), (xr - 2, xc - 2), (xr - 3, xc - 3)]
           ]
        then Just NW
        else Nothing

findXmas :: Grid -> Int
findXmas g = sum [findMas r c g | r <- [0 .. length g - 1], c <- [0 .. length (g !! r) - 1], g !! r !! c == 'X']

main :: IO ()
main = loadInput >>= print . findXmas . lines
