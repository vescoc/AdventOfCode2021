module Lib
    ( someFunc, solve1, solve2
    ) where

import Data.List (stripPrefix, foldl', nub, intercalate)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

someFunc :: IO ()
someFunc = do
  start <- getCurrentTime
  i <- input
  let p1 = solve1 i
  putStrLn $ "part 1: " ++ show p1
  let p2 = solve2 i
  putStrLn $ "part 2: \n" ++ p2
  end <- getCurrentTime
  let delta = diffUTCTime end start
  putStrLn $ "delta: " ++ show delta

input :: IO String
input = readFile "../input"

parseInput :: String -> ([Dot Int], [FoldInstruction Int])
parseInput s = (map read points, map read (drop 1 foldInstructions))
  where (points, foldInstructions) = break (=="") $ lines s

fold :: (Ord a, Num a) => [Dot a] -> FoldInstruction a -> [Dot a]
fold dots fis = nub $ map (fold' fis) dots
  where fold' (FoldX line) (Dot x y)
          | x < line = Dot x y
          | otherwise = Dot (line * 2 - x) y
        fold' (FoldY line) (Dot x y)
          | y < line = Dot x y
          | otherwise = Dot x (line * 2 - y)          

solve1 :: String -> Int
solve1 s = length $ foldl' fold points $ take 1 foldInstructions
  where (points, foldInstructions) = parseInput s

solve2 :: String -> String
solve2 s = format foldInstructions $ foldl' fold points foldInstructions
  where (points, foldInstructions) = parseInput s

format :: (Ord a, Integral a) => [FoldInstruction a] -> [Dot a] -> String
format fis dots = intercalate "\n" [[p x y | x <- [0..maxx - 1]] | y <- [0..maxy - 1]]
  where p x y = if Dot x y `elem` dots then '#' else '.'
        (Dot maxx maxy) = foldl' fold (Dot 0 0) fis
        fold (Dot x y) (FoldX line) = Dot line y
        fold (Dot x y) (FoldY line) = Dot x line

data Dot a = Dot a a
  deriving (Show, Eq, Ord)

data FoldInstruction a = FoldX a | FoldY a
  deriving (Show, Eq)

instance Read a => Read (Dot a) where
  readsPrec d r = [(Dot x y, rs) |
                   (x, rs') <- readsPrec (d + 1) r,
                   (y, rs) <- readsPrec (d + 1) $ drop 1 rs']

instance Read a => Read (FoldInstruction a) where
  readsPrec d r = [(f line, rs) |
                   (f, rs') <- case stripPrefix "fold along x=" r of
                                  Just r' -> [(FoldX, r')]
                                  Nothing -> case stripPrefix "fold along y=" r of
                                               Just r' -> [(FoldY, r')]
                                               Nothing -> [],
                   (line, rs) <- readsPrec (d + 1) rs']
