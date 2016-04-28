module Euler where

import Data.List ((\\), find)

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

-- The sum of [0, 1, 2, 3, ..., n].
sumFrom1toN :: Integer -> Integer
sumFrom1toN n = n * (n + 1) `div` 2

 -- ordered lists difference
minus (x:xs) (y:ys) = case (compare x y) of 
           LT -> x : minus  xs  (y:ys)
           EQ ->     minus  xs     ys 
           GT ->     minus (x:xs)  ys
minus  xs     _     = xs

-- Generate the list of primes up to n using the Sieve of Eratosthenes
primesUpto :: Integer -> [Integer]
primesUpto m = 2 : sieve [3, 5 .. m]
	where
		sieve (x:xs) = x : sieve (xs `minus` [x*x, x*x + 2*x .. m])
		sieve [] = []
