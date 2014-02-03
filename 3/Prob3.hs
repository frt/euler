-- TAH TUDO ERRADO!!!
divisors n = [x | x <- [2..], n `mod` x == 0]

isPrime n = head (divisors n) == n

primos = [n | n <- [2..], (isPrime n)]

primeDivisors n = filter isPrime (divisors n)

greaterPrimeDivisor = head . primeDivisors
