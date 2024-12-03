module Day3.Advanced where

import Day3.Common (Instruction (..), eval, parse)
import Utils.IO (loadInput)

filterEnabled :: Bool -> [Instruction] -> [Instruction]
filterEnabled _ [] = []
filterEnabled _ (Do : rest) = filterEnabled True rest
filterEnabled _ (Dont : rest) = filterEnabled False rest
filterEnabled True (Multiply a b : rest) = Multiply a b : filterEnabled True rest
filterEnabled x (_ : rest) = filterEnabled x rest

main :: IO ()
main = loadInput >>= print . sum . map eval . filterEnabled True . parse
