{-# LANGUAGE DeriveFunctor #-}

module Interpreter where

import Control.Monad.Free

import IR

data Toplevel next
  = Exit Int
  | WriteString String next
  | Error String
  deriving (Functor)

type TL = Free Toplevel ()

exitStatus :: Int -> TL
exitStatus status = Free (Exit status)

exitError :: String -> TL
exitError err = Free (Error err)

writeString :: String -> TL
writeString s = Free (WriteString s (Pure ()))

evalTopLevelIO :: TL -> IO Bool
evalTopLevelIO (Pure _) = return True
evalTopLevelIO (Free cmd) = case cmd of
  Exit i -> do
    return (i == 0)
  WriteString s next -> do
    putStr s
    evalTopLevelIO next
  Error err -> do
    putStrLn err
    return False
