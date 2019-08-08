module Where where

fib = (map f [0..] !!)
  where
    f 0 = 1
    f 1 = 1
    f m = fib (m-2) + fib (m-1)

main = print $ fib 10
