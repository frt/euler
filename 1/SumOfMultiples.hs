-- http://projecteuler.net/problem=1

import System.IO

limit :: Integer
limit = 999

sumDivisibleBy :: Integer -> Integer
sumDivisibleBy n = n * p * (p + 1) `div` 2
	where p = limit `div` n

-- 3*(1+2+3+...+333) + 5*(1+2+3+...+199) - <divisibles by 15>
main = putStrLn $ show $ sumDivisibleBy 3 + sumDivisibleBy 5 - sumDivisibleBy (3 * 5)
