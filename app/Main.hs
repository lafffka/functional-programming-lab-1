module Main where

import System.Random (randomRIO)
import qualified Euler8 (euler8Tail, euler8Recursion, euler8Modular, euler8Map, euler8Cycle, euler8Inf)

generateRandomNDigitInteger :: Int -> IO Integer
generateRandomNDigitInteger n = do
    let
        lowerBound = 10^(n-1)
        upperBound = 10^n - 1
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