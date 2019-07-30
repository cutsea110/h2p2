module Main where

fib_mem :: Int -> Integer
fib_mem = (xs !!)

xs = map fib [0..]

fib 0 = 1
fib 1 = 1
fib n = fib_mem (n-2) + fib_mem (n-1)

main :: IO ()
main = print $ fib_mem 10000
