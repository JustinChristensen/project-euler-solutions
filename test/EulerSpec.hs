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
    describe "Euler.allDivides" $ do
        it "should test that a list of numbers divides a number evenly" $ do
            2520 `shouldSatisfy` (allDivides [1..10])
            2521 `shouldNotSatisfy` (allDivides [1..10])
    describe "Euler.findAllDivides" $ do
        it "should find a number divisible by a list of numbers" $ do
            findAllDivides 10 `shouldBe` Just 2520
    describe "Euler.sumSquareDiff" $ do
        it "should print the difference between the square of sums and sum of squares" $ do
            sumSquareDiff 10 `shouldBe` 2640
    describe "Euler.nDigits" $ do
        it "should return a series of all of the n digit numbers in another number" $ do
            nDigits 1 4355 `shouldBe` [5, 5, 3, 4]
            nDigits 1 1010 `shouldBe` [0, 1, 0, 1]
            nDigits 2 4355 `shouldBe` [55, 35, 43]
            nDigits 4 thousandDigitNumber `shouldSatisfy` elem 9989
    describe "Euler.containsZero" $ do
        it "should test whether or not a number contains zero" $ do
            10101 `shouldSatisfy` containsZero
            1111 `shouldNotSatisfy` containsZero
    describe "Euler.filterContainsZero" $ do
        it "should remove numbers containing zero" $ do
            filterContainsZero [10101, 1111] `shouldBe` [1111]
            filterContainsZero [] `shouldBe` []
            