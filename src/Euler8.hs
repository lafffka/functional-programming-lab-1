module Euler8
    ( euler8Tail
    , euler8Recursion
    , euler8Modular
    , euler8Map
    , euler8Cycle
    , euler8Inf
    ) where

import Data.Char (digitToInt)
import Data.List (tails, foldl')

-- Tail recursion
euler8Tail :: Integer -> Int -> Int
euler8Tail number windowSize =
    maxProduct numbers 0 where
        numbers = map digitToInt (show number)
        maxProduct :: [Int] -> Int -> Int
        maxProduct xs currentMax
            | length xs < windowSize = currentMax
            | otherwise =
                let
                    windowProduct = product (take windowSize xs)
                    newMax = max currentMax windowProduct
                in
                    maxProduct (tail xs) newMax

-- Recursion
euler8Recursion :: Integer -> Int -> Int
euler8Recursion number windowSize =
    maxProduct numbers where
        numbers = map digitToInt (show number)
        maxProduct :: [Int] -> Int
        maxProduct xs
            | length xs < windowSize = 0
            | otherwise = max windowProduct restMax where
                windowProduct = product (take windowSize xs)
                restMax = maxProduct (tail xs)

-- Modular
euler8Modular :: Integer -> Int -> Int
euler8Modular number windowSize =
    let
        digits = map digitToInt (show number)
        windows = map (take windowSize) (tails digits)
        validWindows = filter (\w -> length w == windowSize) windows
        maxProduct = foldl' (\acc x -> max acc (product x)) 0 validWindows
        -- maxProduct = foldl (\acc x -> max acc (product x)) 0 validWindows
        -- maxProduct = maximum (map product validWindows)
    in maxProduct
    
-- Map
euler8Map :: Integer -> Int -> Int
euler8Map number windowSize =
    let 
        digits = map digitToInt (show number)
        len = length digits
        indices = [0 .. len - windowSize]
        windows = map (\i -> take windowSize (drop i digits)) indices
        maxProduct = maximum (map product windows)
    in maxProduct

-- Cycle
euler8Cycle :: Integer -> Int -> Int
euler8Cycle number windowSize =
    let
        digits = map digitToInt (show number)
        len = length digits
        maxProduct = maximum [product (take windowSize (drop i digits)) | i <- [0 .. len - windowSize]]
    in maxProduct

-- Inf
euler8Inf :: Integer -> Int -> Int
euler8Inf number windowSize =
    let
        digits = map digitToInt (show number)
        tailsInf = iterate (drop 1) digits
        hasWindow :: [a] -> Bool
        hasWindow xs = case drop (windowSize - 1) xs of
                            [] -> False
                            _  -> True

        windows = map (take windowSize) (takeWhile hasWindow tailsInf)
    in foldl' max 0 (map product windows)
