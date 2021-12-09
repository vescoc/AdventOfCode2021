module Lib
    ( someFunc, solve1, solve2, shapes, combine
    ) where

import Control.Monad
import Data.List

import Data.Time.Clock (getCurrentTime, diffUTCTime)

someFunc :: IO ()
someFunc = do
  start <- getCurrentTime
  i <- input
  let p1 = solve1 i :: Int
  putStrLn $ "part 1: " ++ show p1
  let p2 = solve2 i :: Int
  putStrLn $ "part 2: " ++ show p2
  end <- getCurrentTime
  let delta = diffUTCTime end start
  putStrLn $ "delta: " ++ show delta

input :: IO String
input = readFile "../input"

solve1 :: String -> Int
solve1 = sum . map (length . filter (`elem` [2, 3, 4, 7]) . map length . parse) . lines
  where parse = words . dropWhile (/= '|')

solve2 :: String -> Int
solve2 = undefined

shapes :: String -> [(Int, [(Char, String)])]
shapes digit
  | l == 2 = [(1, zip digit (repeat one))]
  | l == 3 = [(7, zip digit (repeat seven))]
  | l == 4 = [(4, zip digit (repeat four))]
  | l == 5 = [(2, zip digit (repeat two)), (3, zip digit (repeat three)), (5, zip digit (repeat five))]
  | l == 6 = [(0, zip digit (repeat zero)), (6, zip digit (repeat six)), (9, zip digit (repeat nine))]
  | l == 7 = [(8, zip digit (repeat eight))]
  | otherwise = []
  where
    l = length digit
    zero = "abcefg"
    one = "cf"
    two = "acdeg"
    three = "acdfg"
    four = "bcdf"
    five = "abdfg"
    six = "abdefg"
    seven = "acf"
    eight = "abcdefg"
    nine = "abcdfg"

combine :: [(Int, [(Char, String)])] -> [(Int, [(Char, String)])] -> [[(Int, [(Char, String)])]]
combine pool shape = nub $ do
  (digit1, ms1) <- pool
  (digit2, ms2) <- shape
  guard (digit1 /= digit2)
  case sequence [combineX digit1 ms1 ms2, combineX digit2 ms2 ms1] of
    Just v -> [v]
    Nothing -> []
  where
    combineX digit ms1 ms2 = if any (null . snd) combined then Nothing else Just (digit, combined)
      where 
      combined = do
        (c, s) <- ms1
        case lookup c ms2 of
          Just s2 -> [(c, intersect s s2)]
          Nothing -> [(c, s)]

          
