-- http://projecteuler.net/problem=3

import System.IO (putStrLn)
import System.Environment (getArgs)

-- The greatest integer i that i*i <= n. [http://en.wikipedia.org/wiki/Integer_square_root]
iSqrt :: Integer -> Integer
iSqrt n = iSqrt' n n
	where
		iSqrt' _ 0 = 0
		iSqrt' _ 1 = 1
		iSqrt' n x0 
			| x0 - x1 < 1	= x0
			| otherwise	= iSqrt' n x1
			where 
				x1 = (x0 + (n `div` x0)) `div` 2

-- 
largestPrimeFactor :: Integer -> Integer
largestPrimeFactor n
	| n < 2 = error "Number lesser than 2 cannot be factorized into primes."
	| n `mod` 2 == 0 = factorize (factorize2 n) 3 2
	| otherwise = factorize n 3 1
	where
		factorize2 0 = 0
		factorize2 n 
			| n `mod` 2 == 0 = factorize2 (n `div` 2)
			| otherwise = n
		factorize n factor lastFactor
			| n > 1 && factor <= iSqrt n && n `mod` factor == 0 = factorize (n `div` factor) factor factor
			| n > 1 && factor <= iSqrt n = factorize n (factor + 2) lastFactor
			| n == 1 = lastFactor
			| otherwise = n

main = do
	[arg1] <- getArgs
	let n = read arg1
	putStrLn $ show $ largestPrimeFactor n

