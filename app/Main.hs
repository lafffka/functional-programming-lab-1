module Main where

import Euler23 (euler23Cycle, euler23Inf, euler23Map, euler23Modular, euler23Recursion, euler23Tail)
import qualified Euler8 (euler8Cycle, euler8Inf, euler8Map, euler8Modular, euler8Recursion, euler8Tail)
import System.Random (randomRIO)

generateRandomNDigitInteger :: Int -> IO Integer
generateRandomNDigitInteger n = do
  let lowerBound = 10 ^ (n - 1)
      upperBound = 10 ^ n - 1
  randomRIO (lowerBound, upperBound)

main :: IO ()
main = do
  let digitCount = 10
  number <- generateRandomNDigitInteger digitCount
  putStrLn ("Generated number: " ++ show number)
  let windowSize = 4
      resultTail = Euler8.euler8Tail number windowSize
      resultRecursion = Euler8.euler8Recursion number windowSize
      resultModular = Euler8.euler8Modular number windowSize
      resultMap = Euler8.euler8Map number windowSize
      resultCycle = Euler8.euler8Cycle number windowSize
      resultInf = Euler8.euler8Inf number windowSize
  putStrLn ("Maximum product of " ++ show windowSize ++ " consecutive digits (TailRecursion): " ++ show resultTail)
  putStrLn ("Maximum product of " ++ show windowSize ++ " consecutive digits (Recursion): " ++ show resultRecursion)
  putStrLn ("Maximum product of " ++ show windowSize ++ " consecutive digits (Modular): " ++ show resultModular)
  putStrLn ("Maximum product of " ++ show windowSize ++ " consecutive digits (Map): " ++ show resultMap)
  putStrLn ("Maximum product of " ++ show windowSize ++ " consecutive digits (Cycle): " ++ show resultCycle)
  putStrLn ("Maximum product of " ++ show windowSize ++ " consecutive digits (Inf): " ++ show resultInf)
  putStrLn ("Euler23 (tail): " ++ show euler23Tail)
  putStrLn ("Euler23 (Recursion): " ++ show euler23Recursion)
  putStrLn ("Euler23 (Modular): " ++ show euler23Modular)
  putStrLn ("Euler23 (Map): " ++ show euler23Map)
  putStrLn ("Euler23 (Cycle): " ++ show euler23Cycle)
  putStrLn ("Euler23 (Inf): " ++ show euler23Inf)
