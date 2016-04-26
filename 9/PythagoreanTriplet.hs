-- http://projecteuler.net/problem=9

import System.Environment (getArgs)
import Euler (iSqrt)
import System.IO (putStrLn)
import Data.Maybe (catMaybes)

removeFactorsOf2 :: Integer -> Integer
removeFactorsOf2 sm
	| sm `mod` 2 == 0 = removeFactorsOf2 (sm `div` 2)
	| otherwise = sm


triplet :: Integer -> (Integer, Integer, Integer) -> Maybe (Integer, Integer, Integer)
triplet s2 (m, sm, k)
	| k < 2*m && k <= sm =
		if sm `mod` k == 0 && gcd k m == 1 then Just (a, b, c)
		else triplet s2 ((k + 2), m, sm)
	| otherwise = Nothing
	where
		d = s2 `div` (k * m)
		n = k - m
		a = d * (m * m - n * n)
		b = 2 * d * m * n
		c = d * (m * m + n * n)

pythagoreanTriplet :: Integer -> [(Integer, Integer, Integer)]
pythagoreanTriplet s =
	let 
		s2 = s `div` 2
		mlimit = (iSqrt s2)
		ms = [2 .. mlimit]
		ms' = filter (\m -> s2 `mod` m == 0) ms	-- divisors of s2
		sms = map (s2 `div` ) ms'
		sms' = map removeFactorsOf2 sms	-- reduce the search space by removing all factors 2
		ks = map (\m -> if m `mod` 2 == 1 then m+2 else m+1) ms'
	in
		catMaybes $ map (triplet s2) $ zip3 ms' sms' ks

showResults [] s = putStrLn $ "There's no Pythagorean triplet for which a + b + c = " ++ s ++ "."
showResults triplets@((a, b, c):_) _ = putStrLn $ (show triplets) ++ " : " ++ (show (a*b*c))

main = do
	[s] <- getArgs
	--let (a, b, c) = pythagoreanTriplet (read s)
	showResults (pythagoreanTriplet (read s)) s
