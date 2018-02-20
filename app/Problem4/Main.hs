module Main where

import Euler (largestPalindromeFromProduct)

main :: IO ()
main = let prependAnswer = (++) "The largest palindromic number computed by multiplying two three digit numbers is:\n"
    in putStrLn $ prependAnswer $ show $ largestPalindromeFromProduct 999
