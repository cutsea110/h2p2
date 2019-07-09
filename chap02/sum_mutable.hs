module Main where

import Control.Monad.ST
import Data.STRef

count_st :: Int -> Int
count_st n = runST $ do
  ref <- newSTRef 0
  let go 0 = readSTRef ref
      go i = modifySTRef' ref (+ i) >> go (i - 1)
  go n

count_pure :: Int -> Int
count_pure n = go n 0
  where
    go 0 s = s
    go i s = go (i - 1) $! (s + i)

-- main = print $ count_pure 10000000
main = print $ count_st 10000000
