-- If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

-- NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen)
-- contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.

import Data.Char

written :: Int -> String
written 1    = "one"
written 2    = "two"
written 3    = "three"
written 4    = "four"
written 5    = "five"
written 6    = "six"
written 7    = "seven"
written 8    = "eight"
written 9    = "nine"
written 10   = "ten"
written 11   = "eleven"
written 12   = "twelve"
written 13   = "thirteen"
written 15   = "fifteen"
written 18   = "eighteen"
written 20   = "twenty"
written 30   = "thirty"
written 40   = "forty"
written 50   = "fifty"
written 60   = "sixty"
written 70   = "seventy"
written 80   = "eighty"
written 90   = "ninety"
written 1000 = "one thousand"
written x | div x 10 == 1 = written (mod x 10) ++ "teen"
          | x < 100       = written (10 * div x 10) ++ '-' : written (mod x 10)
          | otherwise     = written (div x 100) ++ " hundred" ++ case mod x 100 of 0 -> ""
                                                                                   x -> " and " ++ written x

keepLetters :: String -> String
keepLetters "" = ""
keepLetters (x:xs) | isLetter x = x : keepLetters xs
                   | otherwise = keepLetters xs

main = do
    print $ sum $ map (length . keepLetters . written) [1..1000]