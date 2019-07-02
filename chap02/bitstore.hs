{-# LANGUAGE BangPatterns #-}
module Main where

-- import Data.Array.Unboxed
-- import Data.Bits (xor)

type BitTuple = (Bool, Bool, Bool)
-- data BitStruct = BitStruct !Bool !Bool !Bool deriving Show
-- type BitArray = UArray Int Bool

go :: BitTuple -> [Int] -> BitTuple
go acc [] = acc
go (two, three, five) (x:xs) = go (test 2 x `xor` two, test 3 x `xor` three, test 5 x `xor` five) xs

True ` xor` False = True
False `xor` True  = True
_     `xor` _     = False

test n x = x `mod` n == 0

main :: IO ()
main = print $ go (False,False,False) [1..1000000]
