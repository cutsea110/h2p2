module Main where

fib_mem_arg :: Int -> Integer
fib_mem_arg n = map fib [0..] !! n
  where
    fib 0 = 1
    fib 1 = 1
    fib m = fib_mem_arg (m-2) + fib_mem_arg (m-1)

main :: IO ()
main = print $ fib_mem_arg 30
