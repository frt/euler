-- http://projecteuler.net/problem=10

import Euler (primesUpto)
import System.Environment (getArgs)
import System.IO (putStrLn)

main = do
	[arg] <- getArgs
	let n = read arg
	putStrLn $ show $ sum $ primesUpto n
