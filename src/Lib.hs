module Lib
    ( someFunc
    ) where

import Control.Monad.Reader
import Control.Monad.State.Lazy

type Program = ReaderT String (StateT String IO)

inIO :: IO a -> Program a
inIO = lift . lift

inState :: (StateT String IO) a -> Program a
inState = lift

myProgram :: Program ()
myProgram = do
  x <- ask
  inIO $ do
    putStrLn $ "Got configuration " ++ x
    putStrLn "Enter a line to update state:"
  input <- inIO getLine
  inState $ put input
  inIO $ putStrLn $ "Updated state with " ++ input
  
someFunc :: IO ()
someFunc = evalStateT program ""
  where program = runReaderT myProgram "String configuration"
