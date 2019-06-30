module Main where

import Data.List (foldl')

sum' = foldl' (+) 0

mean :: [Double] -> Double
mean v = sum' v / fromIntegral (length v)

covariance :: [Double] -> [Double] -> Double
covariance xs ys = sum' (zipWith (\x y -> (x - mean xs) * (y - mean ys)) xs ys) / fromIntegral (length xs)

main = do
  let xs = [1, 1.1 .. 500]
      ys = [2, 2.1 .. 501]
  print $ covariance xs ys
