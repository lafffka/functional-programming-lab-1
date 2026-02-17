module Main (main) where

import Test.Hspec
import qualified Euler8Spec
import qualified Euler23Spec

main :: IO ()
main = hspec $ do
    describe "Problem 8: Largest product in a series" Euler8Spec.spec
    describe "Problem 23: Non-abundant sums" Euler23Spec.spec
