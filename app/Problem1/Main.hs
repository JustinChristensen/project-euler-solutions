module Main where

import Euler (sumMultOf3And5)

main :: IO ()
main = 
    let prependAnswer = (++) "The sum of multiples of 3 and 5 between 3 and 999 is:\n"
    in putStrLn $ prependAnswer $ show $ sumMultOf3And5 [3..999]
