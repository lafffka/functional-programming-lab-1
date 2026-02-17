module Euler23
  ( euler23Tail,
    euler23Recursion,
    euler23Modular,
    euler23Map,
    euler23Cycle,
    euler23Inf,
  )
where

import Data.List (foldl')
import qualified Data.Set as Set

sumDivisorsTail :: Int -> Int
sumDivisorsTail n = go 1 0
  where
    go d acc
      | d > n `div` 2 = acc
      | n `mod` d == 0 = go (d + 1) (acc + d)
      | otherwise = go (d + 1) acc

sumDivisorsRecursion :: Int -> Int -> Int
sumDivisorsRecursion n d
  | d > n `div` 2 = 0
  | n `mod` d == 0 = d + sumDivisorsRecursion n (d + 1)
  | otherwise = sumDivisorsRecursion n (d + 1)

sumDivisorsMod :: Int -> Int
sumDivisorsMod n = foldl' (+) 0 [x | x <- [1 .. n `div` 2], n `mod` x == 0]

sumDivisorsMap :: Int -> Int
sumDivisorsMap n = sum $ map (\d -> if n `mod` d == 0 then d else 0) [1 .. n `div` 2]

isAbundantTail :: Int -> Bool
isAbundantTail n = sumDivisorsTail n > n

isAbundantRec :: Int -> Bool
isAbundantRec n = sumDivisorsRecursion n 1 > n

getAbundantsTail :: Int -> [Int]
getAbundantsTail limit = go 1 []
  where
    go curr acc
      | curr > limit = reverse acc
      | isAbundantTail curr = go (curr + 1) (curr : acc)
      | otherwise = go (curr + 1) acc

getAbundantsRec :: Int -> [Int]
getAbundantsRec n
  | n > 28123 = []
  | isAbundantRec n = n : getAbundantsRec (n + 1)
  | otherwise = getAbundantsRec (n + 1)

getAbdundantsInf :: [Int]
getAbdundantsInf = [x | x <- [1 ..], sum [d | d <- [1 .. x `div` 2], x `mod` d == 0] > x]

isSumOfTwoTail :: Int -> [Int] -> Set.Set Int -> Bool
isSumOfTwoTail n [] _ = False
isSumOfTwoTail n (a : as) abundantSet
  | a > n `div` 2 = False
  | Set.member (n - a) abundantSet = True
  | otherwise = isSumOfTwoTail n as abundantSet

isSumOfTwoRec :: Int -> [Int] -> Set.Set Int -> Bool
isSumOfTwoRec n [] _ = False
isSumOfTwoRec n (a : as) abundantSet
  | a > n `div` 2 = False
  | Set.member (n - a) abundantSet = True
  | otherwise = isSumOfTwoRec n as abundantSet

-- Tail Recursion
euler23Tail :: Int
euler23Tail =
  let limit = 28123
      abundants = getAbundantsTail limit
      abundantSet = Set.fromList abundants
      go n acc
        | n > limit = acc
        | not (isSumOfTwoTail n abundants abundantSet) = go (n + 1) (acc + n)
        | otherwise = go (n + 1) acc
   in go 1 0

-- Recursion
euler23Recursion :: Int
euler23Recursion =
  let limit = 28123
      abundants = getAbundantsRec 1
      abundantSet = Set.fromList abundants

      solve n
        | n > limit = 0
        | not (isSumOfTwoRec n abundants abundantSet) = n + solve (n + 1)
        | otherwise = solve (n + 1)
   in solve 1

-- Modular
euler23Modular :: Int
euler23Modular =
  let limit = 28123
      abundants = filter (\x -> sumDivisorsMod x > x) [1 .. limit]
      abundantSet = Set.fromList abundants
      isSumOfTwo n = any (\a -> Set.member (n - a) abundantSet) (takeWhile (<= n `div` 2) abundants)
      nonSums = filter (not . isSumOfTwo) [1 .. limit]
   in foldl' (+) 0 nonSums

-- Map
euler23Map :: Int
euler23Map =
  let limit = 28123
      abundants = filter (\x -> sumDivisorsMap x > x) [1 .. limit]
      abundantSet = Set.fromList abundants
      isSumOfTwo n = any (\a -> Set.member (n - a) abundantSet) (takeWhile (<= n `div` 2) abundants)
      resultsSequence = map (\i -> if isSumOfTwo i then 0 else i) [1 .. limit]
   in sum resultsSequence

-- List Comprehension
euler23Cycle :: Int
euler23Cycle =
  let limit = 28123
      sumDivs n = sum [d | d <- [1 .. n `div` 2], n `mod` d == 0]
      abundants = [x | x <- [1 .. limit], sumDivs x > x]
      abundantSet = Set.fromList abundants
      isSumOfTwo n = or [Set.member (n - a) abundantSet | a <- takeWhile (<= n `div` 2) abundants]
      nonSums = [n | n <- [1 .. limit], not (isSumOfTwo n)]
   in sum nonSums

-- Infinite list
euler23Inf :: Int
euler23Inf =
  let limit = 28123
      abundants = takeWhile (<= limit) getAbdundantsInf
      abundantSet = Set.fromList abundants
      isSumOfTwo n = any (\a -> Set.member (n - a) abundantSet) (takeWhile (<= n `div` 2) abundants)
      nonSums = filter (not . isSumOfTwo) [1 .. limit]
   in sum nonSums
