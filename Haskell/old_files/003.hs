-- The prime factors of 13195 are 5, 7, 13 and 29.

-- What is the largest prime factor of the number 600851475143 ?

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime x = not $ or $ map (==0) [mod x p | p <- takeWhile ((<= sqrt (fromIntegral x)) . fromIntegral) primes]

primes = filter isPrime [1..]

primeFactors :: Int -> [Int]
primeFactors x = checkFactors x primes
    where checkFactors 1 _ = []
          checkFactors n pp@(p:ps) | mod n p == 0 = p : checkFactors (div n p) pp
                                   | otherwise    = checkFactors n ps

main = do
    print $ maximum $ primeFactors 600851475143