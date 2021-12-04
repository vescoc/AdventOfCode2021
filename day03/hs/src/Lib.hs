module Lib
    ( someFunc, solve1, solve2
    ) where

import Data.Bits (Bits, shift, (.|.))
import Data.List (foldl')
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

input :: IO [String]
input = do
  ds <- readFile "../input"
  return $ lines ds

solve1 :: (Bits a, Num a) => [String] -> a
solve1 xs = let r = map (fromBool . (>0)) $ foldr1 reduce $ map convert xs
                gammaRate = toNum r
                epsilonRate = toNum $ negate r
            in gammaRate * epsilonRate
  where reduce = zipWith (+)
        fromBool True = 1
        fromBool False = 0
        convert = map convert'
          where convert' '0' = -1
                convert' '1' = 1
        negate = map (1-)

toNum :: (Bits a, Num a) => [a] -> a
toNum = foldl' toNum' 0
  where toNum' acc d = (acc `shift` 1) .|. d

solve2 :: (Bits a, Num a, Ord a) => [String] -> a
solve2 xs = let oxygenGeneratorRating = reduce xs mostCommonCriteria 0
                co2ScrubberRating = reduce xs leastCommonCriteria 0
            in oxygenGeneratorRating * co2ScrubberRating
  where
    reduce [l] _ _ = toNum $ map toDigit l
      where toDigit '0' = 0
            toDigit '1' = 1
    reduce ys bitCriteria bit = reduce (filter (\y -> y !! bit == target) ys) bitCriteria (bit + 1)
      where
        target = bitCriteria $ sum $ map (\y -> if y!!bit == '1' then 1 else -1) ys
    mostCommonCriteria x
      | x >= 0 = '1'
      | otherwise = '0'
    leastCommonCriteria x
      | x >= 0 = '0'
      | otherwise = '1'
