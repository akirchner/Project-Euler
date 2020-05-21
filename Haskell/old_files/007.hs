-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

-- What is the 10001st prime number?

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime x = not $ or $ map (==0) [mod x p | p <- takeWhile ((<= sqrt (fromIntegral x)) . fromIntegral) primes]

primes = filter isPrime [1..]

main = do
    print $ primes !! 10000