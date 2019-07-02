{-# LANGUAGE GADTs #-}
module Main where

data Object a where
  Number :: Integral a => a -> Object a
  Character :: Char -> Object Char

main = print $ foldl (+) 0 [1..1000000 :: Int]
-- main = print $ foldl (\a (Number b) -> a + b) 0 [Number x | x <- [1..1000000 :: Int]] -- add type signature is very important!!
