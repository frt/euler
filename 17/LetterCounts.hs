-- http://projecteuler.net/problem=17

import System.IO

teen = length "teen"
hundred = length "hundred"
thousand = length "thousand"
and' = length "and"

letterCount 1 = length "one"
letterCount 2 = length "two"
letterCount 3 = length "three"
letterCount 4 = length "four"
letterCount 5 = length "five"
letterCount 6 = length "six"
letterCount 7 = length "seven"
letterCount 8 = length "eight"
letterCount 9 = length "nine"
letterCount 10 = length "ten"
letterCount 11 = length "eleven"
letterCount 12 = length "twelve"
letterCount 13 = length "thirteen"
letterCount 15 = length "fifteen"
letterCount 18 = length "eighteen"
letterCount 20 = length "twenty"
letterCount 30 = length "thirty"
letterCount 40 = length "forty"
letterCount 50 = length "fifty"
letterCount 60 = length "sixty"
letterCount 70 = length "seventy"
letterCount 80 = length "eighty"
letterCount 90 = length "ninety"
letterCount n
	| 10 < n && n < 20 = letterCount unit + teen
	| 20 < n && n < 100 = letterCount (dec * 10) + letterCount (unit)
	| 100 <= n && n < 1000 = letterCount hund + hundred + if (n `mod` 100) > 0 then and' + letterCount (n `mod` 100) else 0
	| otherwise = letterCount thou + thousand + if (n `mod` 1000) > 0 then and' + letterCount (n `mod` 1000) else 0
	where 
		unit = n `div` 1 `mod` 10
		dec  = n `div` 10 `mod` 10
		hund = n `div` 100 `mod` 10
		thou = n `div` 1000 `mod` 10

main = putStrLn $ show $ (sum . map letterCount) [1..1000]
