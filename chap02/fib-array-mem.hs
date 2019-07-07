import Data.Array

fib :: Int -> Array Int Integer
fib n = arr
  where
    arr = listArray (1,n) $ 1:1:[arr!(i-2)+arr!(i-1)|i<-[3..n]]
