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
            sumMultOf3And5 [1..9] `shouldBe` 23
    describe "Euler.sumEvenFibonacci" $ do
        it "should sum even fibonacci numbers below 15" $ do
            sumEvenFibonacci (< 15) `shouldBe` 10
    describe "Euler.largestPrimeFactor" $ do
        it "should find the largest prime factor of 13195" $ do
            largestPrimeFactor 13195 `shouldBe` 29
    describe "Euler.countDigits" $ do
        it "should count the number of digits in an integer" $ do
            countDigits 12345 `shouldBe` 5
            countDigits (-123) `shouldBe` 3
            countDigits 0 `shouldBe` 1
    describe "Euler.numReversal" $ do
        it "should reverse an integer" $ do
            numReversal 123 `shouldBe` 321
            numReversal (-532) `shouldBe` (-235)
            numReversal 0 `shouldBe` 0
    describe "Euler.isPalindrome" $ do
        it "should detect palindromic numbers" $ do
            121 `shouldSatisfy` isPalindrome
            1135 `shouldNotSatisfy` isPalindrome
    describe "Euler.decrProductListFrom" $ do
        it "should generate a list of products" $ do
            decrProductListFrom 3 `shouldBe` [9,6,6,4,3,3,2,2,1]
    describe "Euler.largestPalindromeFromProduct" $ do
        it "should find the largest palindrome from the product of two two-digit numbers" $ do
            largestPalindromeFromProduct 99 `shouldBe` 9009


            
