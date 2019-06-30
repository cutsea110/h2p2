{-# LANGUAGE BangPatterns #-}

mySum :: [Int] -> Int
mySum [] = 0
mySum (x:xs) = x + mySum xs

mySum2 :: Int -> [Int] -> Int
mySum2 s [] = s
mySum2 s (x:xs) = let s' = s + x in s' + mySum2 s' xs

mySum3 :: [Int] -> Int -> Int
mySum3 [] !s = s
mySum3 (x:xs) !s = mySum3 xs (x + s)

mySum4 :: [Int] -> Int -> Int
mySum4 [] s = id $! s
mySum4 (x:xs) s = mySum3 xs $! x + s
