-- http://projecteuler.net/problem=25

import System.IO

fibonacciSequence :: [Integer]
fibonacciSequence = 0:1:[fibonacciSequence!!(i-2)+fibonacciSequence!!(i-1) | i <- [2..]]

main = putStrLn $ show $ fst . head $ filter (\f -> (length . show . snd) f >= 1000) (zip [0..] fibonacciSequence)
