module Lib
    ( someFunc, solve1, solve2, parse
    ) where

import Data.Time.Clock (getCurrentTime, diffUTCTime)

someFunc :: IO ()
someFunc = do
  start <- getCurrentTime
  i <- input
  let p1 = solve1 i :: Int
  putStrLn $ "part 1: " ++ show p1
  let p2 = solve2 i :: Integer
  putStrLn $ "part 2: " ++ show p2
  end <- getCurrentTime
  let delta = diffUTCTime end start
  putStrLn $ "delta: " ++ show delta

input :: IO String
input = readFile "../input"

solve :: Integral a => Int -> [a] -> a
solve n i = sum $ lanternfishes !! n
  where
    lanternfishes = iterate evolve initial
    evolve [v0, v1, v2, v3, v4, v5, v6, v7, v8] = [v1, v2, v3, v4, v5, v6, v7 + v0, v8, v0]
    initial = foldr add1 (replicate 9 0) i
      where add1 idx v = let idx' = fromInteger . toInteger $ idx
                         in take idx' v ++ [(v !! idx') + 1] ++ drop (idx' + 1) v
      
      
solve1 :: (Read a, Integral a) => String -> a
solve1 = solve 80 . parse

solve2 :: (Read a, Integral a) => String -> a
solve2 = solve 256 . parse

parse :: (Read a) => String -> [a]                               
parse xs = do
  (x, r) <- reads xs
  x : parse (drop 1 r)
