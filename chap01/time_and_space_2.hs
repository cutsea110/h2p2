module Main where

goGen u = sum [1..u] + product [1..u]
goGenShared u = let xs = [1..u] in sum xs + product xs

goGenOnePass u = su + pu
  where
    (su,pu) = foldl f (0,1) [1..u]
    f (s,p) i = let s' = s+i
                    p' = p*i
                in s' `seq` p' `seq` (s',p')

main = print $ goGenOnePass 10000
