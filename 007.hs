-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

-- What is the 10001st prime number?

isPrime :: Int -> Bool -- change this to use primes.
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime x = not $ or $ map (==0) [mod x p | p <- takeWhile (<= div x 2) primes]

primes = filter isPrime [1..]

p007 = do
    print $ primes !! 10000