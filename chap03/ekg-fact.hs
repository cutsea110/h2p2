{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import System.Remote.Monitoring
import System.Metrics
import qualified System.Metrics.Counter as Counter

main :: IO ()
main = do
  server <- forkServer "localhost" 8000
  factorials <- createCounter "factorials.count" (serverMetricStore server)

  forever $ do
    input <- getLine
    print $ product [1..read input :: Integer]
    Counter.inc factorials
