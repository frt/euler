-- http://projecteuler.net/problem=4

import System.IO (putStrLn)
import System.Environment (getArgs)
import Data.Maybe (listToMaybe)

isPalindrome :: Integer -> Bool
isPalindrome n = (show n) == ((reverse . show) n)

largestPalindrome :: Integer -> Integer -> Integer
largestPalindrome min max = foldl (iter max) 0 [max, max - 1 .. min]

iter :: Integer -> Integer -> Integer -> Integer
iter bMax largestPalindrome a =
	let 
		prods = [prod | b <- [bMax, (bMax-1) .. a], let prod = a * b, prod > largestPalindrome, isPalindrome prod]
	in
		case listToMaybe prods of
			Nothing -> largestPalindrome
			Just bs -> head prods

main = do
	[arg1, arg2] <- getArgs
	let min = read arg1
	    max = read arg2
	putStrLn $ show $ largestPalindrome min max
