module Main where

import Euler (largestPrimeFactor)

main :: IO ()
main = let prependAnswer = (++) "The largest prime factor of 600851475143 is:\n"
    in putStrLn $ prependAnswer $ show $ largestPrimeFactor 600851475143
