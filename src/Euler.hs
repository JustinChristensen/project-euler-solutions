module Euler (
    divides,
    multOf3And5,
    sumMultOf3And5,
    sumEvenFibonacci,
    largestPrimeFactor
    ) where

-- Yes, I'm aware this is cheating
import Math.NumberTheory.Primes.Factorisation

divides :: (Integral a) => a -> a -> Bool
divides d n = n `mod` d == 0

multOf3And5 :: (Integral i) => i -> Bool
multOf3And5 i = 3 `divides` i || 5 `divides` i

sumMultOf3And5 :: (Integral a) => [a] -> a
sumMultOf3And5 xs = sum $ filter multOf3And5 xs

fibs :: (Num a) => [a]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

sumEvenFibonacci :: (Integral a) => (a -> Bool) -> a
sumEvenFibonacci continue = sum $ takeWhile continue $ filter even fibs 

largestPrimeFactor :: Integer -> Integer
largestPrimeFactor n = fst $ last $ factorise n