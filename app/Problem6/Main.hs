module Main where

import Euler (sumSquareDiff)
import Data.Maybe

main :: IO ()
main = let prependAnswer = (++) "The difference between the square of sums and the sum of squares up to 100 is\n"
    in putStrLn $ prependAnswer $ show $ sumSquareDiff 100
