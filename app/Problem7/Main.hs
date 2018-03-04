module Main where

import Math.NumberTheory.Primes.Sieve (primes)

main :: IO ()
main = let prependAnswer = (++) "The 10001st prime number is:\n"
    in putStrLn $ prependAnswer $ show $ last $ take 10001 primes