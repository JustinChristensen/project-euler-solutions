module Main where

import Euler (greatestProduct, thousandDigitNumber)

main :: IO ()
main = 
    let 
        prependAnswer = (++) "The product of the thirteen adjacent digits in the number that have the greatest product is:\n"
    in putStrLn $ prependAnswer $ show $ greatestProduct thousandDigitNumber