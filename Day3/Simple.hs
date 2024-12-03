module Day3.Simple where

import Day3.Common (eval, parse)
import Utils.IO (loadInput)

main :: IO ()
main = loadInput >>= print . sum . map eval . parse
