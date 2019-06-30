module Main where

goGen u = sum [1..u] + product [1..u]
goGenShared u = let xs = [1..u] in sum xs + product xs

main = print $ goGenShared 10000
