-- n! means n × (n − 1) × ... × 3 × 2 × 1

-- For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
-- and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

-- Find the sum of the digits in the number 100!

import Data.Char

fact :: Integral a => a -> a
fact 0 = 1
fact n = n * fact (n - 1)

getDigits :: Integer -> [Int]
getDigits n = map (\x -> ord x - ord '0') $ show n

p020 = do
    print $ sum $ getDigits $ fact 100