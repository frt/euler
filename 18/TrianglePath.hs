-- http://projecteuler.net/problem=18

import System.IO

type Triangle = [[Integer]]

triangleFromString :: String -> Triangle
triangleFromString = map (map read . words) . lines

maximumPathSum :: Triangle -> Triangle
maximumPathSum t = maximumPathSum' 0
	where
		maximumPathSum' 0 = [t!!0!!0] : maximumPathSum' 1
		maximumPathSum' r 
			| r < length t = [greaterSum r c | c <- [0..r]] : maximumPathSum' (r+1)
			| otherwise    = [] 

		greaterSum r c = t!!r!!c + max (previousLeft) (previousRight)
			where
				previousLeft  = if c-1 < 0 then 0 else t'!!(r-1)!!(c-1)
				previousRight = if c > r-1 then 0 else t'!!(r-1)!!c
				t' = maximumPathSum t

-- Usage:
-- ./TrianglePath < triangle.in
main = do
	str <- getContents
	let triangle = triangleFromString str
	putStrLn $ show $ (maximum . last) (maximumPathSum triangle)
