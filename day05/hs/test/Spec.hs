import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Lib (solve1, solve2, point, line)

main :: IO ()
main = hspecWith defaultConfig { configFastFail = True } specs

specs :: Spec
specs = do
  describe "day05" $ do
    it "read Point" $ do
      read "0,0" `shouldBe` point 0 0
    it "read Line" $ do
      read "0,0 -> 10,10" `shouldBe` line (point 0 0) (point 10 10)
    it "solve1" $ do
      solve1 input `shouldBe` 5
    it "solve2" $ do
      solve2 input `shouldBe` 12

input :: String
input = "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2"
