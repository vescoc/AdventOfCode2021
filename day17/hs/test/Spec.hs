import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Lib (solve1, solve2)

main :: IO ()
main = hspecWith defaultConfig { configFastFail = True } specs

specs :: Spec
specs = do
  describe "day17" $ do
    it "solve1" $ do
      solve1 input `shouldBe` 45
    it "solve2" $ do
      solve2 input `shouldBe` 112

input :: String
input = "target area: x=20..30, y=-10..-5"
