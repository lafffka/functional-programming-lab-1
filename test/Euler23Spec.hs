module Euler23Spec (spec) where

import Euler23 (euler23Cycle, euler23Inf, euler23Map, euler23Modular, euler23Recursion, euler23Tail)
import Test.Hspec

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
