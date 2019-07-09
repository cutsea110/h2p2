module Main where

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Vector.Unboxed as V -- use unboxed vector
import Data.Vector.Unboxed.Mutable as MV
import System.Random (randomIO) -- for testing

bubblesortM :: (Ord a, PrimMonad m, Unbox a) => MVector (PrimState m) a -> m () -- require Ord
bubblesortM v = loop
  where
    indices = V.enumFromTo 1 (MV.length v - 1) -- use V.enumFromTo
    loop = do swapped <- V.foldM' f False indices 
              if swapped then loop else return ()
    f swapped i = do
        a <- MV.unsafeRead v (i-1) -- use MV.unsafeRead
        b <- MV.unsafeRead v i     -- use MV.unsafeRead
        if a > b
          then MV.unsafeSwap v (i-1) i >> return True  -- use MV.unsafeSwap
          else return swapped

bubblesort :: (Unbox a, Ord a) => Vector a -> Vector a -- require Unbox
bubblesort v = runST $ do
    mv <- V.thaw v
    bubblesortM mv
    V.unsafeFreeze mv -- use V.unsafeFreeze

main = do
    v <- V.generateM 10000 $ \_ -> randomIO :: IO Double
    let v_sorted = bubblesort v
        median   = v_sorted ! 5000
    print median
