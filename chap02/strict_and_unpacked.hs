{-# LANGUAGE BangPatterns #-}
module Main where

data PairP = PairP Int Int deriving Show

data PairS = PairS !Int !Int deriving Show

data PairU = PairU {-# UNPACKED #-} !Int {-# UNPACKED #-} !Int deriving Show

iter :: Int -> (a -> a) -> a -> a
iter end f x = go 0 x
  where go !n x | n < end   = go (n+1) $! f x
                | otherwise = x

-- main = print $ iter 1000 (\(PairP x y) -> PairP y (x+y)) (PairP 1 1)
-- main = print $ iter 1000 (\(PairS x y) -> PairS y (x+y)) (PairS 1 1)
main = print $ iter 1000 (\(PairU x y) -> PairU y (x+y)) (PairU 1 1)
