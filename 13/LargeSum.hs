-- https://projecteuler.net/problem=13

import System.IO

main = do
	input <- getContents	
	let nums = map (read . take 11) (lines input) :: [Integer]
	putStrLn $ show (sum nums)
