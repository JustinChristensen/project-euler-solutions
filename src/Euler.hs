module Euler (
    divides,
    multOf3And5,
    sumMultOf3And5,
    sumEvenFibonacci,
    largestPrimeFactor,
    countDigits,
    numReversal,
    isPalindrome,
    palindromes,
    decrProductListFrom,
    largestPalindromeFromProduct
) where

-- Yes, I'm aware this is cheating
import Math.NumberTheory.Primes.Factorisation
import Data.List

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

countDigits' :: (Integral a) => a -> a
countDigits' n
    | q == 0 = 1
    | otherwise = 1 + countDigits' q
    where q = n `quot` 10

countDigits :: (Integral a) => a -> a
countDigits n =
    if n < 0 then countDigits' (abs n) 
    else countDigits' n

numReversal' :: (Integral a) => a -> a -> a
numReversal' n z = 
    let (q, r) = n `quotRem` 10
    in 
        if z == 0 then n
        else r * 10 ^ z + numReversal' q (z - 1)

numReversal :: (Integral a) => a -> a
numReversal n = 
    let z = countDigits n
    in numReversal' n (z - 1)

isPalindrome :: (Show a, Integral a) => a -> Bool
isPalindrome i = 
    let i' = show i
    in i' == reverse i'

palindromes :: (Show a, Integral a) => [a]
palindromes = filter isPalindrome [0..]

decrProductListFrom :: (Show a, Integral a) => a -> [a]
decrProductListFrom i = 
    let e = i `quot` 10 + 1
        i' = i - 1
    in sortBy (flip compare) [x * y | x <- [i,i'..e], y <- [i,i'..e]]

largestPalindromeFromProduct :: (Show a, Integral a) => a -> a
largestPalindromeFromProduct i = head $ filter isPalindrome $ decrProductListFrom i



