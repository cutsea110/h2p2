{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
module Main where

import Control.Monad.Writer

newtype DList a = DList ([a] -> [a])

fromList :: [a] -> DList a
fromList xs = DList (xs ++)

toList :: DList a -> [a]
toList (DList list) = list []

instance Semigroup (DList a) where
  DList x <> DList y = DList (x . y)

instance Semigroup (DList a) => Monoid (DList a) where
  mempty = DList id

type DListWriter = Writer (DList Int)
type ListWriter  = Writer [Int]

action :: Int -> ListWriter ()
action 15000 = return ()
action n     = action (n+1) >> tell [n]

action' :: Int -> DListWriter ()
action' 15000 = return ()
action' n     = action' (n+1) >> tell (fromList [n])

main = forM (snd $ runWriter (action 1)) print
-- main = forM (toList $ snd $ runWriter (action' 1)) print
