module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = do
  i <- input
  let p1 = solve1 i
  putStrLn $ "part 1: " ++ show p1
  let p2 = solve2 i
  putStrLn $ "part 2: " ++ show p2

input :: Read a => IO [a]
input = do
  i <- readFile "../input"
  return $ map read $ lines i

solve1 :: [Int] -> Int
solve1 xs = solve1' 0 xs
  where solve1' :: Int -> [Int] -> Int
        solve1' i [] = i
        solve1' i [_] = i
        solve1' i (x1:x2:xs)
          | x2 > x1 = solve1' (i + 1) (x2:xs)
          | otherwise = solve1' i (x2:xs)

solve2 :: (Ord a, Num a) => [a] -> Int
solve2 = length . filter (\[x, y] -> y > x) . windows 2 . map sum . windows 3
  where windows n xs
          | length xs >= n = (take n xs):(windows n $ drop 1 xs)
          | otherwise = []
