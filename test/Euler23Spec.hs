module Euler23Spec (spec) where

import Test.Hspec
import Euler23 (euler23Tail, euler23Recursion, euler23Modular, euler23Map, euler23Cycle, euler23Inf)

testAnswer :: Int
testAnswer = 4179871

spec :: Spec
spec = do
    it "matches the project Euler solution" $ do
        euler23Tail `shouldBe` testAnswer
        euler23Recursion `shouldBe` testAnswer
        euler23Modular `shouldBe` testAnswer
        euler23Map `shouldBe` testAnswer
        euler23Cycle `shouldBe` testAnswer
        euler23Inf `shouldBe` testAnswer
