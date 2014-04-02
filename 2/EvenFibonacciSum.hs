-- http://projecteuler.net/problem=2

import System.IO

-- Every third element in the fibonacci series is even, 
-- so the computation can be simplified.
-- This algorithm uses memoize compute the series faster.
evenFibonacciSeries :: [Integer]
evenFibonacciSeries = 2:8:[4 * evenFibonacciSeries!!(i - 1) + evenFibonacciSeries!!(i - 2) | i <- [2..]]

main = putStrLn $ show $ sum $ takeWhile (<= 4000000) evenFibonacciSeries
