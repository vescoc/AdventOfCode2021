module Lib
    ( someFunc, solve1, solve2
    ) where

import Data.Time.Clock (getCurrentTime, diffUTCTime)

someFunc :: IO ()
someFunc = do
  start <- getCurrentTime
  i <- input
  putStrLn $ "part 1: " ++ (show $ solve1 i)
  putStrLn $ "part 2: " ++ (show $ solve2 i)
  end <- getCurrentTime
  putStrLn $ "delta: " ++ (show $ diffUTCTime end start)

input :: IO String
input = readFile "../input"

solve :: (Integral a) => (a -> a) -> [a] -> a
solve f xs = minimum [sum dx | i <- [0..max], let dx = map (distance i) xs]
  where
    max = maximum xs
    distance p = f . abs . (p-)

solve1 :: (Read a, Integral a) => String -> a
solve1 = solve id . parse

solve2 :: (Read a, Integral a) => String -> a
solve2 = solve (\n -> n * (n + 1) `div` 2) . parse

parse :: (Read a) => String -> [a]
parse s = do
  (x, r) <- reads s
  x : parse (drop 1 r)
