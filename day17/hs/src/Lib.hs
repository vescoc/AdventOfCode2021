module Lib
    ( someFunc, solve1, solve2
    ) where

import Data.List (stripPrefix)
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

solve1 :: String -> Int
solve1 s = maximum $ map (\(Probe _ vy _ _) -> vy * (vy + 1) `div` 2) $ throws rx ry
  where Right (rx, ry) = parseInput s

solve2 :: String -> Int
solve2 s = length $ throws rx ry
  where Right (rx, ry) = parseInput s

step :: Probe -> Probe
step (Probe 0 vy x y) = Probe 0 (vy - 1) x (vy + y)
step (Probe vx vy x y) = Probe (vx - 1) (vy - 1) (vx + x) (vy + y)

parseInput :: String -> Either String (Range, Range)
parseInput s = do
  (x, r) <- case stripPrefix "target area: x=" s of
              Just v -> case reads v of
                          [(range, rs)] -> Right (range, rs)
                          _ -> Left "Invalid x range"
              Nothing -> Left "Expecting x range"
  (y, _) <- case stripPrefix ", y=" r of
              Just v -> case reads v of
                          [(range, rs)] -> Right (range, rs)
                          _ -> Left "Invalid y range"
              Nothing -> Left "Expecting y range"
  return (x, y)

throws :: Range -> Range -> [Probe]
throws (Range xmin xmax) (Range ymin ymax) = filter inRange [Probe vx vy 0 0 | vx <- [1 .. xmax], vy <- [ymin .. -ymin]]
  where inRange p@(Probe vx _ x y)
          | x + vx * (vx + 1) `div` 2 < xmin = False
          | x > xmax || y < ymin = False
          | x `elem` [xmin .. xmax] && y `elem` [ymin .. ymax] = True
          | otherwise = inRange $ step p

data Probe = Probe Int Int Int Int
  deriving Show

data Range = Range Int Int
  deriving Show

instance Read Range where
  readsPrec d r = do
    (start, r') <- readsPrec (d + 1) r
    r'' <- case stripPrefix ".." r' of
             Just rs -> return rs
             Nothing -> []
    (end, r''') <- readsPrec (d + 1) r''
    return (Range start end, r''')
