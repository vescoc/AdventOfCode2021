module Lib
    ( someFunc, solve1, solve2, point, line, Point, Line, draw
    ) where

import Control.Monad
import Data.List (stripPrefix, sort, group)
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

solve :: (Line Int -> Bool) -> String -> Int
solve g i = length $ filter (>1) $ map length $ group $ sort $ do
  lx <- lines i
  let l = read lx :: Line Int
  guard (g l)
  draw l

solve1 :: String -> Int
solve1 = solve (\l -> isVertical l || isHorizontal l)

solve2 :: String -> Int
solve2 = solve (const True)

point :: a -> a -> Point a
point = Point

line :: Point a -> Point a -> Line a
line = Line

isVertical :: Eq a => Line a -> Bool
isVertical (Line (Point x1 _) (Point x2 _)) = x1 == x2

isHorizontal :: Eq a => Line a -> Bool
isHorizontal (Line (Point _ y1) (Point _ y2)) = y1 == y2

draw :: (Eq a, Num a) => Line a -> [Point a]
draw (Line (Point x1 y1) (Point x2 y2)) = draw' x1 y1
  where
    dx = signum $ x2 - x1
    dy = signum $ y2 - y1
    draw' x y
      | x == x2 && y == y2 = [Point x y]
      | otherwise = Point x y : draw' (x + dx) (y + dy)

data Point a = Point a a
  deriving (Eq, Ord, Show)

data Line a = Line (Point a) (Point a)
  deriving (Eq, Show)

instance Read a => Read (Point a) where
  readsPrec d r = [(Point x y, rs''') |
                   (x, rs') <- readsPrec (d + 1) r,
                   rs'' <- maybe [] return $ stripPrefix "," rs',
                   (y, rs''') <- readsPrec (d + 1) rs'']

instance Read a => Read (Line a) where
  readsPrec d r = [(Line p1 p2, rs''') |
                   (p1, rs') <- readsPrec (d + 1) r,
                   rs'' <- maybe [] pure $ stripPrefix " -> " rs',
                   (p2, rs''') <- readsPrec (d + 1) rs'']
                   
