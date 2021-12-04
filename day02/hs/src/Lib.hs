module Lib
    ( someFunc
    ) where

import Data.List (foldl')
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Control.Monad

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

data Command a = Forward a
  | Down a
  | Up a
  deriving Show

instance Read a => Read (Command a) where
  readsPrec d r = [(c n, rs) |
                   (l, s) <- lex r,
                   c <- case l of
                      "forward" -> [Forward]
                      "down" -> [Down]
                      "up" -> [Up]
                      _ -> [],
                   (n, rs) <- readsPrec (d + 1) s]

input :: Read a => IO [Command a]
input = do
  i <- readFile "../input"
  return $ map read $ lines i

solve :: (b -> a) -> (b -> Command a -> b) -> b -> [Command a] -> a
solve result apply init = result . foldl' apply init 

solve1 :: Num a => [Command a] -> a
solve1 = solve mul apply (0, 0)
  where apply (h, v) (Forward n) = (h + n, v)
        apply (h, v) (Down n) = (h, v + n)
        apply (h, v) (Up n) = (h, v - n)
        mul (h, v) = h * v

solve2 :: Num a => [Command a] -> a
solve2 = solve mul apply (0, 0, 0)
  where apply (h, v, a) (Forward n) = (h + n, v + a * n, a)
        apply (h, v, a) (Down n) = (h, v, a + n)
        apply (h, v, a) (Up n) = (h, v, a - n)
        mul (h, v, _) = h * v

newtype State s a = State { runState :: s -> (a, s) }

state :: (s -> (a, s)) -> State s a
state = State

instance Functor (State s) where
  fmap = liftM


instance Applicative (State s) where
  pure = return
  
  (<*>) = ap


instance Monad (State s) where
  return x = state $ \s -> (x, s)

  -- (>>=) :: State s a -> (a -> State s b) -> State s b
  p >>= k = q
    where
      p' = runState p
      k' = runState . k
      q' s0 = (y, s2)
        where
          (x, s1) = p' s0
          (y, s2) = k' x s1
      q = state q'
