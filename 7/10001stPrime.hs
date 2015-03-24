-- http://projecteuler.net/problem=7

import Euler (iSqrt)
import System.IO (putStrLn)
import System.Environment (getArgs)

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime 3 = True
isPrime n
	| even n         = False
	| n < 9          = True
	| n `mod` 3 == 0 = False
	| otherwise      = all ((/= 0) . (n `mod`)) [6 * k + l | k <- [1 .. (iSqrt n) `div` 6 + 1], l <- [-1, 1]]

main = do
	[arg1] <- getArgs
	let n = read arg1
	putStrLn $ show $ (filter isPrime [1..]) !! (n - 1)
