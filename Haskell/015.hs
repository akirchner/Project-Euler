-- Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.

-- How many such routes are there through a 20×20 grid?

numPaths :: Int -> Int -> Int -- takes three days, need a better method
numPaths 0 0 = 0
numPaths 0 _ = 1
numPaths _ 0 = 1
numPaths x y = numPaths (x - 1) y + numPaths x (y - 1)

p015 = do
    print $ numPaths 20 20