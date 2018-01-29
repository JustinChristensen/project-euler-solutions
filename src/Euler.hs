module Euler
    (divides
    ,multOf3And5
    ,sumMultOf3And5
    ) where

divides :: (Integral a) => a -> a -> Bool
divides d n = n `mod` d == 0

multOf3And5 :: (Integral i) => i -> Bool
multOf3And5 i = 3 `divides` i || 5 `divides` i

sumMultOf3And5 :: (Integral a) => [a] -> a
sumMultOf3And5 xs = sum $ filter multOf3And5 xs
