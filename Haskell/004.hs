-- A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

-- Find the largest palindrome made from the product of two 3-digit numbers.

isPalindromic :: Int -> Bool
isPalindromic = isPalindrome . show
    where isPalindrome "" = True
          isPalindrome [c] = True
          isPalindrome (c:cs) | c == (last cs) = isPalindrome $ init cs
                              | otherwise      = False

main = do
    print $ maximum $ filter isPalindromic [x * y | x <- [100..999], y <- [100..999]]