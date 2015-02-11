-- http://projecteuler.net/problem=6

import Euler (sumFrom1toN)
import System.IO (putStrLn)
import System.Environment (getArgs)

square = (^2)

-- (1 + 2 + 3 + ... + n)^2
squareOfSum :: Integer -> Integer
squareOfSum = square . sumFrom1toN

-- 1^2 + 2^2 + 3^2 + ... + n^2
sumOfSquares :: Integer -> Integer
sumOfSquares n = (2*n + 1) * (n + 1) * n `div` 6

sumSquareDifference :: Integer -> Integer
sumSquareDifference n = squareOfSum n - sumOfSquares n

main = do
	[arg1] <- getArgs
	let n = read arg1
	putStrLn $ show $ sumSquareDifference n
