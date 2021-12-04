import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Lib (solve1, solve2)
main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do
  describe "day03" $ do
    it "solve1" $ do
      solve1 input `shouldBe` (198 :: Int)
    it "solve2" $ do
      solve2 input `shouldBe` (230 :: Int)

input :: [String]
input = lines "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"
