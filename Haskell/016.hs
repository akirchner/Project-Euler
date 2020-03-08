-- 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

-- What is the sum of the digits of the number 2^1000?

import Data.Char

getDigits :: Integer -> [Int]
getDigits n = map (\x -> ord x - ord '0') $ show n

p016 = do
    print $ sum $ getDigits $ 2 ^ 1000