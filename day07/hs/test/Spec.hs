import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Lib (solve1, solve2)

main :: IO ()
main = hspecWith defaultConfig { configFastFail = True } specs

specs :: Spec
specs = do
  describe "day07" $ do
    it "solve1" $ do
      solve1 input `shouldBe` 37
    it "solve2" $ do
      solve2 input `shouldBe` 168

input :: String
input = "16,1,2,0,4,2,7,1,2,14"
