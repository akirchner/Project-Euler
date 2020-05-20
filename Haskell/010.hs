-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

-- Find the sum of all the primes below two million.

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime x = not $ or $ map (==0) [mod x p | p <- takeWhile ((<= sqrt (fromIntegral x)) . fromIntegral) primes]

primes = filter isPrime [1..]

main = do
    print $ sum $ takeWhile (< 2000000) primes