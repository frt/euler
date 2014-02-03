fib 1 = 1
fib 2 = 2
fib n = fib (n - 1) + fib (n - 2)

fib_series = [fib n | n <- [1..]]
