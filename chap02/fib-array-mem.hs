import Data.Array

fib :: Int -> Array Int Integer
fib n = arr
  where
    arr = listArray (1,n) $ 1:1:[arr!(i-2)+arr!(i-1)|i<-[3..n]]

pascal :: Int -> Array (Int, Int) Integer
pascal n = arr
  where
    arr = array ((1,1), (n,n)) $
      [ ((i,1), 1) | i <- [1..n]] ++
      [ ((1,j), 1) | j <- [1..n]] ++
      [ ((i,j), arr!(i-1, j) + arr!(i, j-1)) | i <- [2..n], j <- [2..n]]
