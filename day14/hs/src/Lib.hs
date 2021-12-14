module Lib
    ( someFunc, solve1, solve2
    ) where

import Data.List (sort)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

someFunc :: IO ()
someFunc = do
  start <- getCurrentTime
  i <- input
  let p1 = solve1 i
  putStrLn $ "part 1: " ++ show p1
  let p2 = solve2 i
  putStrLn $ "part 2: " ++ show p2
  end <- getCurrentTime
  let delta = diffUTCTime end start
  putStrLn $ "delta: " ++ show delta

input :: IO String
input = readFile "../input"

parseInput :: String -> (String, [(String, Char)])
parseInput s = (polymer, rules)
  where (polymer', rules') = break (=="") $ lines s
        polymer = head polymer'
        rules = map (parseRule . words) $ drop 1 rules'
        parseRule [h, _, t] = (h, head t)
        parseRule _ = undefined

solve :: Int -> String -> Integer
solve steps s = result $ map snd $ foldr accumulate [] $ makeStep steps initial
  where (polymer, rules) = parseInput s
        initial = foldr (add 1) [] $ windows polymer
        makeStep 0 pairs = pairs
        makeStep n pairs = makeStep (n - 1) $ foldr (step rules) pairs pairs
        accumulate ([c1, _], v) xs = add v c1 xs
        accumulate _ _ = undefined
        result values = last values' - head values' + 1
          where values' = sort values

solve1 :: String -> Integer
solve1 = solve 10

solve2 :: String -> Integer
solve2 = solve 40

add :: Eq a => Integer -> a -> [(a, Integer)] -> [(a, Integer)]
add v p [] = [(p, v)]
add v p ((p1, c):xs)
  | p == p1 = (p, c + v):xs
  | otherwise = (p1, c):add v p xs

windows :: [a] -> [[a]]
windows (c1:c2:xs) = [c1, c2]:windows (c2:xs)
windows _ = []

step :: [(String, Char)] -> (String, Integer) -> [(String, Integer)] -> [(String, Integer)]
step rules ([c1, c3], counts) = let (Just c2) = lookup [c1, c3] rules
                                in add (-counts) [c1, c3] . add counts [c2, c3] . add counts [c1, c2]
step _ _ = undefined
