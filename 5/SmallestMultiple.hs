-- http://projecteuler.net/problem=5

import Data.List
import Euler (iSqrt)
import System.Environment (getArgs)
import System.IO (putStrLn)
import Data.List (foldl')

-- Generate the list of primes up to n using the Sieve of Eratosthenes
primesUpto :: Integer -> [Integer]
primesUpto n = nextCut 2 [2 .. n]
	where
		nextCut p l = 
			let 
				l' = cutMultiples p l 
			in 
				case find (> p) l' of
					Nothing -> l'	-- the last cut
					Just p' -> nextCut p' l'

		cutMultiples _ [] = []
		cutMultiples m l = l \\ [m * 2, m * 3 .. last l]
		
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
