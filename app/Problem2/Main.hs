module Main where

import Euler (sumEvenFibonacci)

main :: IO ()
main = let prependAnswer = (++) "The sum of even Fibonacci numbers less than four million is:\n"
    in putStrLn $ prependAnswer $ show $ sumEvenFibonacci (< 4000000)
