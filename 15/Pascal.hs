-- http://projecteuler.net/problem=15
-- http://mathworld.wolfram.com/LatticePath.html

import System.IO

-- Interestingly, the number of paths for the point (i, i) is the i'th 
-- central element of the Pascal's Triangle.
pascalCenter :: Integer -> Integer
pascalCenter i = (product [(i + 1)..(i * 2)]) `div` (product [1..i])

main = putStrLn $ show $ pascalCenter 20
