module Lib
    ( sumMultOf3And5
    ) where

isMultOf3And5 :: (Integral i) => i -> Bool
isMultOf3And5 i = 
    let eq0 = (==) 0
        modi = mod i
    in (eq0 $ modi 3) || (eq0 $ modi 5)

sumMultOf3And5 :: (Integral a) => [a] -> a
sumMultOf3And5 xs = sum $ filter isMultOf3And5 xs