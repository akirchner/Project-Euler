-- Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
-- If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.

-- For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of
-- 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

-- Evaluate the sum of all the amicable numbers under 10000.

propDivisors :: Int -> [Int]
propDivisors 0 = []
propDivisors 1 = []
propDivisors x = 1 : combine l1 (reverse l2)
    where factorPairs = [(m, n) | m <- [2..(ceiling $ sqrt $ fromIntegral x)], m^2 <= x, mod x m == 0, let n = div x m]
          (l1, l2) = unzip factorPairs
          combine [] _ = []
          combine xs l@(y:ys) | last xs == y = xs ++ ys
                              | otherwise    = xs ++ l

isAmicable :: Int -> Bool
isAmicable x = let y = sum $ propDivisors x
               in if x == y then False else sum (propDivisors y) == x

main = do
    print $ sum $ filter isAmicable [1..9999]