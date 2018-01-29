import Test.Hspec
import Euler
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
    describe "Euler.divides" $ do
        it "should determine if an integral is divisible by another integral" $ do
            15 `shouldSatisfy` (3 `divides`)
        it "should throw when provided a 0 divisor" $ do
            evaluate (0 `divides` 15) `shouldThrow` anyArithException
    describe "Euler.multOf3And5" $ do
        it "should determine if an integral is a multiple of either 3 or 5" $ do
            12 `shouldSatisfy` multOf3And5
            11 `shouldNotSatisfy` multOf3And5
    describe "Euler.sumMultOf3And5" $ do
        it "should sum the multiples of 3 or 5 in a list" $ do
            (sumMultOf3And5 [1..9]) `shouldBe` 23
    describe "Euler.sumEvenFibonacci" $ do
        it "should sum even fibonacci numbers below 15" $ do
            (sumEvenFibonacci (< 15)) `shouldBe` 10

            
