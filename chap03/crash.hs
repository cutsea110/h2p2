module Main where

f x = head x
g = f . tail
h = g . tail

main = print $ h [1,2]
