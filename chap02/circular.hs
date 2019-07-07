module Main where

import Data.Sequence as Seq
import Data.Foldable (toList)
import Data.List (foldl')

data Circular a = Circular !Int (Seq.Seq a)

create :: Int -> Circular a
create n = Circular n Seq.empty

values :: Circular a -> [a]
values (Circular _ s) = toList s

observe :: Circular a -> a -> Circular a
observe (Circular n s) x
  | Seq.length s < n   = Circular n $ s  |> x
  | _ :< s' <- viewl s = Circular n $ s' |> x

main = print $ values $ foldl' observe (create 7) [1..10000000 :: Int]
