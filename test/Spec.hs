module Main (main) where

import qualified Euler23Spec
import qualified Euler8Spec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Problem 8: Largest product in a series" Euler8Spec.spec
  describe "Problem 23: Non-abundant sums" Euler23Spec.spec
