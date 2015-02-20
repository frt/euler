-- http://projecteuler.net/problem=5

import Data.List

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
