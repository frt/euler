-- http://projecteuler.net/problem=14

import System.IO

collatz_next :: Integer -> Integer
collatz_next n
	| n `mod` 2 == 0 = n `div` 2
	| otherwise = 3 * n + 1

collatz_chain_length :: Integer -> Integer
collatz_chain_length 1 = 1
collatz_chain_length n = 1 + collatz_chain_length (collatz_next n)

main = putStrLn $ show $ maximum [(collatz_chain_length n, n) | n <- [1..1000000]]
