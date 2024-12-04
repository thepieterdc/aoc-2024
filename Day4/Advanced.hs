module Day4.Advanced where

import Utils.IO (loadInput)

findMs :: Int -> Int -> [String] -> Bool
findMs ar ac g = length (filter (\x -> map char x == "MS") [findNE, findSE, findSW, findNW]) == 2
  where
    char (r', c') = if r' >= 0 && r' < length g && c' >= 0 && c' < length (g !! r') then g !! r' !! c' else '.'
    findNE =
      [ (r, c)
        | (r, c) <- [(ar + 1, ac - 1), (ar - 1, ac + 1)]
      ]
    findSE =
      [ (r, c)
        | (r, c) <- [(ar - 1, ac - 1), (ar + 1, ac + 1)]
      ]
    findSW =
      [ (r, c)
        | (r, c) <- [(ar - 1, ac + 1), (ar + 1, ac - 1)]
      ]
    findNW =
      [ (r, c)
        | (r, c) <- [(ar + 1, ac + 1), (ar - 1, ac - 1)]
      ]

findXmas :: [String] -> Int
findXmas g = length [(r, c) | r <- [0 .. length g - 1], c <- [0 .. length (g !! r) - 1], g !! r !! c == 'A', findMs r c g]

main :: IO ()
main = loadInput >>= print . findXmas . lines
