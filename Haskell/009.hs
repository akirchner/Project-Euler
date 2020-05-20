-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

-- a^2 + b^2 = c^2
-- For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

findPythagoreanTriples :: Int -> [(Int, Int, Int)]
findPythagoreanTriples x = [(a, b, c) | a <- [1..(x-2)], b <- [a..(x-a-1)], let c = x - a - b, a^2 + b^2 == c^2]

product3Tuple :: Num a => (a, a, a) -> a
product3Tuple (a, b, c) = a * b * c

main = do
    print $ product3Tuple $ head $ findPythagoreanTriples 1000