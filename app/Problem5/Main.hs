module Main where

import Euler (findAllDivides)
import Data.Maybe

main :: IO ()
main = let prependAnswer = (++) "The smallest number that is evenly divisible by numbers from 1 to 20 is:\n"
    in putStrLn $ prependAnswer $ show $ fromMaybe 0 $ findAllDivides 20
