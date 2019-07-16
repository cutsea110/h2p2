{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad.State.Strict
import Control.Monad.Cont

newtype StateCPS s r a = StateCPS (Cont (s -> r) a) deriving (Functor, Applicative, Monad, MonadCont)

instance MonadState s (StateCPS s r) where
  get          = StateCPS $ cont $ \next curState -> next curState curState
  put newState = StateCPS $ cont $ \next curState -> next () newState

runStateCPS :: StateCPS s s () -> s -> s
runStateCPS (StateCPS m) = runCont m (\_ -> id)

action :: MonadState Int m => m ()
action = replicateM_ 1000000 $ do
  i <- get
  put $! i+1

main = do
  print $ (runStateCPS action 0 :: Int)
  print $ (snd $ runState action 0 :: Int)
