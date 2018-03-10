module Euler (
    divides,
    multOf3And5,
    sumMultOf3And5,
    sumEvenFibonacci,
    largestPrimeFactor,
    countDigits,
    numReversal,
    isPalindrome,
    palindromes,
    decrProductListFrom,
    largestPalindromeFromProduct,
    allDivides,
    findAllDivides,
    sumSquareDiff,
    nDigits,
    thousandDigitNumber,
    filterContainsZero,
    containsZero,
    greatestProduct
) where

-- Yes, I'm aware this is cheating
import Math.NumberTheory.Primes.Factorisation
import Data.List

divides :: (Integral a) => a -> a -> Bool
divides d n = n `mod` d == 0

multOf3And5 :: (Integral i) => i -> Bool
multOf3And5 i = 3 `divides` i || 5 `divides` i

sumMultOf3And5 :: (Integral a) => [a] -> a
sumMultOf3And5 xs = sum $ filter multOf3And5 xs

fibs :: (Num a) => [a]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

sumEvenFibonacci :: (Integral a) => (a -> Bool) -> a
sumEvenFibonacci continue = sum $ takeWhile continue $ filter even fibs 

largestPrimeFactor :: Integer -> Integer
largestPrimeFactor n = fst $ last $ factorise n

countDigits' :: (Integral a) => a -> a
countDigits' n
    | q == 0 = 1
    | otherwise = 1 + countDigits' q
    where q = n `quot` 10

countDigits :: (Integral a) => a -> a
countDigits n =
    if n < 0 then countDigits' (abs n) 
    else countDigits' n

numReversal' :: (Integral a) => a -> a -> a
numReversal' n z = 
    let (q, r) = n `quotRem` 10
    in 
        if z == 0 then n
        else r * 10 ^ z + numReversal' q (z - 1)

numReversal :: (Integral a) => a -> a
numReversal n = 
    let z = countDigits n
    in numReversal' n (z - 1)

isPalindrome :: (Show a, Integral a) => a -> Bool
isPalindrome i = 
    let i' = show i
    in i' == reverse i'

palindromes :: (Show a, Integral a) => [a]
palindromes = filter isPalindrome [0..]

decrProductListFrom :: (Show a, Integral a) => a -> [a]
decrProductListFrom i = 
    let e = i `quot` 10 + 1
        i' = i - 1
    in sortBy (flip compare) [x * y | x <- [i,i'..e], y <- [i,i'..e]]

largestPalindromeFromProduct :: (Show a, Integral a) => a -> a
largestPalindromeFromProduct i = head $ filter isPalindrome $ decrProductListFrom i

allDivides :: (Foldable t, Integral a) => t a -> a -> Bool
allDivides divs i = all (`divides` i) divs

findAllDivides :: (Integral a) => a -> Maybe a
findAllDivides i = find (allDivides [1..i]) [1..]

sumSquareDiff :: (Integral a) => a -> a
sumSquareDiff i = sum [1..i] ^ 2 - (sum $ map (^ 2) [1..i])

thousandDigitNumber = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450

nDigits :: (Integral a) => a -> a -> [a]
nDigits n d 
    | n <= 0 = []
    | otherwise = 
        let 
            r = (10 ^ n)
            y = r `div` 10 - 1
            nDigits' n d 
                | d <= y = []
                | otherwise = 
                    let z = d `mod` n
                    in z : nDigits' n (d `div` 10)
        in nDigits' r d

containsZero :: (Show a, Integral a) => a -> Bool
containsZero = elem '0' . show

filterContainsZero :: (Show a, Integral a) => [a] -> [a]
filterContainsZero is = filter (not . containsZero) is 

greatestProduct :: (Read a, Show a, Integral a) => a -> a
greatestProduct n = 
    let digits = nDigits 1
        mapSort ns = map (read . sort . show) ns
        greatestProduct' = foldr (*) 1 . digits . maximum . mapSort . filterContainsZero . nDigits 13
    in greatestProduct' n