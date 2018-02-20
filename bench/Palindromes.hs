import Criterion.Main
import Euler

main = 
    let sumNPalindromes n = sum $ take n $ palindromes
    in defaultMain [
        bgroup "palindromes" [ 
            bench "5000" $ whnf sumNPalindromes 5000
        ]
    ]