import System.IO

readGrid :: String -> [[Int]]
readGrid = map ((map read) . words) . lines

diagonals :: [[Int]] -> [[Int]]
diagonals m = [[m!!(i+k)!!(j+k) | k <- [0..3]] | i <- [0..16], j <- [0..16]]

diagonals' :: [[Int]] -> [[Int]]
diagonals' m = [[m!!(i+k)!!(j-k) | k <- [0..3]] | i <- [0..16], j <- [3..19]]

horizontals :: [[Int]] -> [[Int]]
horizontals m = [[m!!i!!(j+k) | k <- [0..3]] | i <- [0..19], j <- [0..16]]

verticals :: [[Int]] -> [[Int]]
verticals m = [[m!!(i+k)!!j | k <- [0..3]] | i <- [0..16], j <- [0..19]]

adjacents m = (diagonals m) ++ (diagonals' m) ++ (horizontals m) ++ (verticals m)

main = do
	file <- getContents
	let grid = readGrid file
	putStrLn $ show $ maximum ( map product (adjacents grid) )
