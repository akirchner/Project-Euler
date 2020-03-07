-- The following iterative sequence is defined for the set of positive integers:

-- n → n/2 (n is even)
-- n → 3n + 1 (n is odd)

-- Using the rule above and starting with 13, we generate the following sequence:

-- 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
-- It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

-- Which starting number, under one million, produces the longest chain?

-- NOTE: Once the chain starts the terms are allowed to go above one million.

collatzLength :: Int -> Int
collatzLength x = f 1 x
    where f n x | x == 1    = n
                | even x    = f (n + 1) (div x 2)
                | otherwise = f (n + 1) (3 * x + 1)

maxKey :: Ord b => [(a, b)] -> a
maxKey (t:ts) = f t ts
    where f m [] = fst m
          f m (x:xs) | snd x > snd m = f x xs
                     | otherwise     = f m xs

p014 = do
    print $ maxKey $ zip [1..999999] $ map collatzLength [1..999999]