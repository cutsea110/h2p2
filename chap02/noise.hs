module Main where

import ListT
import System.Random
import Control.Monad.Trans (lift)
import Control.Concurrent (threadDelay)

noise :: [Double] -> ListT IO Double
noise pat = do
  pat' <- ListT.repeat pat
  x    <- ListT.fromFoldable pat'
  lift $ do delay <- randomIO
            threadDelay (delay `mod` 300000)
            randomRIO (x - 0.5, x + 0.5)

main = let generator = noise [1,5,10,5]
           in ListT.traverse_ print generator
