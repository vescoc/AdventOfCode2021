import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Lib (solve1, solve2, parse)

main :: IO ()
main = hspecWith defaultConfig { configFastFail = True } specs

specs :: Spec
specs = do
  describe "day06" $ do
    it "parse" $ do
      parse input `shouldBe` [3, 4, 3, 1, 2]
    it "solve1" $ do
      solve1 input `shouldBe` 5934
    it "solve2" $ do
      solve2 input `shouldBe` 26984457539

input :: String
input = "3,4,3,1,2"
