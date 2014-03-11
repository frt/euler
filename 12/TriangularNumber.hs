-- http://projecteuler.net/problem=12

import Data.List
import System.IO

-- The sum of [0, 1, 2, 3, ..., n] is the nth triangular number.
triangularNumber :: Integer -> Integer
triangularNumber n = (n * (n + 1)) `div` 2

-- The greatest integer i that i*i <= n.
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

divides :: Integer -> Integer -> Bool
divides d n = n `mod` d == 0

numberOfDivisors :: Integer -> Integer
numberOfDivisors n = foldl sumDivides 0 [1..(iSqrt n)]
	where
		sumDivides accum x
			| x * x == n	= accum + 1
			| x `divides` n	= accum + 2
			| otherwise	= accum

-- The number of divisors of n will be at most (iSqrt n)*2, so we can start
-- from the first triangular number greater than (n*n)/2 to find a triangular
-- number with more than n divisors.  See that the nth triangular number is 
-- (n * (n+1))/2 and ((n-1) * n)/2 < (n*n)/2 for positive n.  So
-- we could assume that all triangular numbers prior to the 501th will have
-- less than 501 divisors, so we don't need to calculate them.
triangularNumbersAbove501 :: [Integer]
triangularNumbersAbove501 = [triangularNumber n | n <- [501..]]

firstTriangleNumberToHaveOverFiveHundredDivisors = fst . head . dropWhile ((< 501) . snd) $ map (\x -> (x, numberOfDivisors x)) triangularNumbersAbove501

main = putStrLn $ show $ firstTriangleNumberToHaveOverFiveHundredDivisors
