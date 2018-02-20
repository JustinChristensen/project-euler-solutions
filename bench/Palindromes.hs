import Criterion.Main
import Euler

main = 
    let sumNPalindromes n = sum $ take n $ palindromes
        sumNPalindromesS n = sum $ take n $ palindromesS
    in defaultMain [
        bgroup "palindromes" [ 
            bench "5000" $ whnf sumNPalindromes 5000
        ],
        bgroup "palindromesS" [ 
            bench "5000" $ whnf sumNPalindromesS 5000
        ]
    ]
