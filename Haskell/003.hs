-- The prime factors of 13195 are 5, 7, 13 and 29.

-- What is the largest prime factor of the number 600851475143 ?

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime x = not $ or $ map (==0) [mod x p | p <- [2..(div x 2)]]

primes = filter isPrime [1..]

primeFactors :: Int -> [Int]
primeFactors x = checkFactors x primes
    where checkFactors 1 _ = []
          checkFactors n pp@(p:ps) | mod n p == 0 = p : checkFactors (div n p) pp
                                   | otherwise    = checkFactors n ps

maxList :: Ord a => [a] -> a
maxList (x:xs) = maxList' x xs
    where maxList' m [] = m
          maxList' m (n:ns) | n > m     = maxList' n ns
                            | otherwise = maxList' m ns

p003 = do
    print $ maxList $ primeFactors 600851475143