import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Lib (solve1, solve2)

main :: IO ()
main = hspecWith defaultConfig { configFastFail = True } specs

specs :: Spec
specs = do
  describe "day10" $ do
    it "solve1" $ do
      solve1 input `shouldBe` 26397
    it "solve2" $ do
      solve2 input `shouldBe` 288957

input :: String
input = "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]"
