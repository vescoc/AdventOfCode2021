module Lib
    ( someFunc, solve1, solve2, Bingo, Board, input, chunks, toDrawOrder, rank, rounds, boards
    ) where

import Control.Monad (join)

import Data.List (elemIndex, minimumBy, maximumBy)
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

solve :: (((Int, Int) -> (Int, Int) -> Ordering) -> [(Int, Int)] -> (Int, Int)) -> String -> Int
solve f i = sumRem * draw
  where
    Bingo rs bs = read i :: Bingo Int
    brs = map (toDrawOrder rs) bs
    (w, mr) = f (\(_, r1) (_, r2) -> r1 `compare` r2) $ zip [0..] $ map rank brs
    Board wb = bs !! w
    sumRem = sum $ map fst $ filter snd $ zip wb (map (>mr) (brs!!w))
    draw = rs !! mr

solve1 :: String -> Int
solve1 = solve minimumBy

solve2 :: String -> Int
solve2 = solve maximumBy

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)

toDrawOrder :: Eq a => [a] -> Board a -> [Int]
toDrawOrder rs (Board bs) = let Just r = mapM (`elemIndex` rs) bs
                            in r

rank :: [Int] -> Int
rank xs = minimum $ join [colMax, rowMax]
  where
    c5 = chunks 5 xs
    rowMax = map maximum c5
    colMax = [maximum x | i <- [0..4], let x = fmap (!!i) c5]

data Bingo a = Bingo {
  rounds :: [a],
  boards :: [Board a]
}
  deriving Show

newtype Board a = Board { board :: [a] }
  deriving Show

instance Read a => Read (Bingo a) where
  readsPrec d r = [(Bingo dos bs, "") |
                   let (dos, xr') = readValues,
                   not . null $ dos,
                   let bs = readBoards xr']
    where
      readValues = readValues' r
        where
          readValues' :: Read a => String -> ([a], String)
          readValues' ('\n':'\n':rs) = ([], rs)
          readValues' (',':rs) = readValues' rs
          readValues' rs = let [(l, rs')] = lex rs
                               (ls, rs'') = readValues' rs'
                           in (read l:ls, rs'')
      readBoards "" = []
      readBoards rs = let [(b, rs')] = readsPrec (d + 1) rs
                      in b:readBoards rs'

instance Read a => Read (Board a) where
  readsPrec _ r = [(Board xs, xr) |
                   let (xs, xr) = readValues,
                   not . null $ xs]
    where
      readValues = readValues' r
        where
          readValues' :: Read a => String -> ([a], String)
          readValues' [] = ([], "")
          readValues' ('\n':'\n':rs) = ([], rs)
          readValues' ('\n':rs) = readValues' rs
          readValues' rs = let [(l, rs')] = lex rs
                               (ls, rs'') = readValues' rs'
                           in (read l:ls, rs'')
            
                    
