-- Used for `deriving (Functor)` on the TopLevel type:
{-# LANGUAGE DeriveFunctor #-}

module TopLevel where

import Control.Monad.Free

data Toplevel next
  = Exit Int
  | WriteString String next
  | Error String
  deriving (Functor)

type TL a = Free Toplevel a

exitStatus :: Int -> TL a
exitStatus status = Free (Exit status)

exitSuccess :: TL a
exitSuccess = exitStatus 0

exitError :: String -> TL a
exitError err = Free (Error err)

writeString :: String -> TL ()
writeString s = Free (WriteString s (Pure ()))

evalTopLevelIO :: TL a -> IO Bool
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