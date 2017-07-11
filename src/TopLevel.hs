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
exitStatus status = liftFree (Exit status)

exitSuccess :: TL a
exitSuccess = exitStatus 0

exitError :: String -> TL a
exitError err = liftFree (Error err)

writeString :: String -> TL ()
writeString s = liftFree (WriteString s ())

liftFree :: (Functor f) => f r -> Free f r
liftFree command = Free (fmap Pure command)

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
