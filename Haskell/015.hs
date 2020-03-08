-- Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.

-- How many such routes are there through a 20×20 grid?

getPaths :: Int -> Int -> Int
getPaths 0 _ = 1
getPaths _ 0 = 1
getPaths x y = paths !! (y - 1) !! x + paths !! y !! (x - 1)

paths = [[getPaths a b | a <- [0..]] | b <- [0..]]

p015 = do
    print $ getPaths 20 20