-- The sum of the squares of the first ten natural numbers is,

-- 1^2+2^2+...+10^2=385

-- The square of the sum of the first ten natural numbers is,

-- (1+2+...+10)^2=55^2=3025

-- Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025−385=2640.

-- Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

sumOfSquares :: Int -> Int
sumOfSquares n = foldr1 (+) [x^2 | x <- [1..n]]

squareOfSum :: Int -> Int
squareOfSum n = (sum [1..n])^2

main = do
    print $ squareOfSum 100 - sumOfSquares 100