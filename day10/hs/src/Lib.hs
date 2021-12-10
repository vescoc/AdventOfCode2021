module Lib
    ( someFunc, solve1, solve2
    ) where

import Data.List (foldl', sort)
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

solve :: (Num a) => ([a] -> a) -> (Either Error Bool -> a) -> (Either Error Bool -> Bool) -> String -> a
solve extractor scoring filtering = extractor . map scoring . filter filtering . map parse . lines

solve1 :: (Num a) => String -> a
solve1 = solve sum score invalidChar
  where
    invalidChar (Left (InvalidChar _)) = True
    invalidChar _ = False
    score (Left (InvalidChar ')')) = 3
    score (Left (InvalidChar ']')) = 57
    score (Left (InvalidChar '}')) = 1197
    score (Left (InvalidChar '>')) = 25137
    score _ = 0

solve2 :: (Ord a, Num a) => String -> a
solve2 = solve middle score incomplete
  where
    incomplete (Left (Incomplete _)) = True
    incomplete _ = False
    middle xs = let sx = sort xs
                in sx !! fromIntegral (div (length sx) 2)
    score (Left (Incomplete xs)) = foldl' score' 0 xs
    score _ = 0
    score' acc c = acc * 5 + cs
      where
        cs
          | c == '(' = 1
          | c == '[' = 2
          | c == '{' = 3
          | c == '<' = 4
          | otherwise = 0

parse :: String -> Either Error Bool
parse = parse' []
  where
    parse' [] [] = Right True
    parse' ss [] = Left $ Incomplete ss
    parse' ('(':ss) (')':xs) = parse' ss xs
    parse' ('[':ss) (']':xs) = parse' ss xs
    parse' ('{':ss) ('}':xs) = parse' ss xs
    parse' ('<':ss) ('>':xs) = parse' ss xs
    parse' ss ('(':xs) = parse' ('(':ss) xs
    parse' ss ('[':xs) = parse' ('[':ss) xs
    parse' ss ('{':xs) = parse' ('{':ss) xs
    parse' ss ('<':xs) = parse' ('<':ss) xs
    parse' _ (x:_) = Left $ InvalidChar x

data Error = InvalidChar Char
           | Incomplete String
