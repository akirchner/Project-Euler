-- If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

-- Find the sum of all the multiples of 3 or 5 below 1000.

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge xx@(x:xs) yy@(y:ys) | x == y = x : merge xs ys
                          | x < y  = x : merge xs yy
                          | y < x  = y : merge xx ys

p001 = do
    print $ sum $ merge [3, 6..999] [5, 10..999]