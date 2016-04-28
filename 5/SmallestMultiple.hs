-- http://projecteuler.net/problem=5

import Euler (iSqrt, primesUpto)
import System.Environment (getArgs)
import System.IO (putStrLn)
import Data.List (foldl')
		
primeFactorExponent :: Integer -> Integer -> Integer
primeFactorExponent n p
	| p > iSqrt n = 1
	| otherwise = floor (log (fromIntegral n) / log (fromIntegral p))

smallestMultiple :: Integer -> Integer
smallestMultiple n = foldl' (\accum p -> accum * p ^ primeFactorExponent n p) 1 (primesUpto n)

main = do
	[arg] <- getArgs
	let n = read arg
	putStrLn $ show $ smallestMultiple n
