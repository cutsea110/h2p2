{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Array.Unboxed
import Data.Bits (xor)
{--
type BitTuple = (Bool, Bool, Bool)

go :: BitTuple -> [Int] -> BitTuple
go acc [] = acc
go (two, three, five) (x:xs) = go (test 2 x `xor` two, test 3 x `xor` three, test 5 x `xor` five) xs

test n x = x `mod` n == 0

main :: IO ()
main = print $ go (False,False,False) [1..1000000]
--}

{--
data BitStruct = BitStruct !Bool !Bool !Bool deriving Show

go :: BitStruct -> [Int] -> BitStruct
go acc [] = acc
go (BitStruct two three five) (x:xs) = go (BitStruct (test 2 x `xor` two) (test 3 x `xor` three) (test 5 x `xor` five)) xs

test n x = x `mod` n == 0

main :: IO ()
main = print $ go (BitStruct False False False) [1..1000000]
--}

type BitArray = UArray Int Bool

go :: BitArray -> [Int] -> BitArray
go arr [] = arr
go arr (x:xs) = let arr' = (listArray (0,2) [ test 2 x `xor` (arr!0)
                                            , test 3 x `xor` (arr!1)
                                            , test 5 x `xor` (arr!2)
                                            ])
                in arr' `seq` go arr' xs

test n x = x `mod` n == 0

main :: IO ()
main = undefined -- print $ go (BitStruct False False False) [1..1000000]
