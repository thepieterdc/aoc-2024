module Day7.Common where

import Utils.IO (loadInput)
import Utils.Parser (Parser, digits, doParse, some, token)

parse :: Parser (Int, [Int])
parse = do n <- digits; token ':'; xs <- some parseNum; return (read n, xs)
  where
    parseNum = do token ' '; read <$> digits

run :: Bool -> String -> Int
run allowConcat = sum . map fst . filter snd . map (uncurry (solve allowConcat Nothing) . doParse parse) . lines

solve :: Bool -> Maybe Int -> Int -> [Int] -> (Int, Bool)
-- Final case
solve _ (Just current) target [] = (target, current == target)
-- Bound case
solve _ (Just current) target _ | current > target = (target, False)
-- Recursive case
solve allowConcat (Just current) target (x : xs) = (target, mulResult || addResult || concatResult)
  where
    mulResult = snd $ solve allowConcat (Just (current * x)) target xs
    addResult = snd $ solve allowConcat (Just (current + x)) target xs
    concatValue = read (show current ++ show x) :: Int
    concatResult = allowConcat && snd (solve True (Just concatValue) target xs)
-- Initial case
solve allowConcat Nothing target (a : b : rest) = (target, mulResult || addResult || concatResult)
  where
    mulResult = snd $ solve allowConcat (Just (a * b)) target rest
    addResult = snd $ solve allowConcat (Just (a + b)) target rest
    concatValue = read (show a ++ show b) :: Int
    concatResult = allowConcat && snd (solve True (Just concatValue) target rest)
