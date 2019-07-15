{-# LANGUAGE DeriveFunctor #-}
module Main where

import Prelude hiding (read)
import Control.Monad.Free

data Language next = Write String next
                   | Read (String -> next)
                   | Missiles (IO ()) next
                   deriving (Functor)

type Program = Free Language

read :: Program String
read = liftF (Read id)

write :: String -> Program ()
write string = liftF (Write string ())

missiles :: IO () -> Program ()
missiles io = liftF (Missiles io ())

program :: Program Int
program = do
  write "Waiting for command (launch)"
  input <- read
  case input of
    "launch" -> do missiles $ putStrLn "Missiles launched!"
                   return 0
    _        -> do write $ "Unknown command: " ++ input
                   program

interpret :: Program a -> IO a
interpret (Pure res)     = return res
interpret (Free program) = case program of
  Write string next -> putStrLn string >> interpret next
  Read go           -> getLine >>= interpret . go
  Missiles m next   -> m >> interpret next

main = interpret program
