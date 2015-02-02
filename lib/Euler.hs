module Euler where

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
