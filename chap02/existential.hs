{-# LANGUAGE GADTs #-}
module Main where

data ObjectE where
  NumberE :: Integral a => a -> ObjectE

main = print $ foldl (\a (NumberE b) -> a + fromIntegral b) 0 [NumberE x | x <- [1..1000000 :: Int]]
