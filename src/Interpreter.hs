{-# LANGUAGE DeriveFunctor #-}

module Interpreter where

import Control.Monad.Free
import Data.Map
import qualified Data.Map as Map

import IR

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

evalTopLevelIO :: TL () -> IO Bool
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

data Context = Context { args :: [Value]
                       , locals :: Map String Value
                       } 


setLocal :: String -> Value -> Context -> Context
setLocal name value ctx = ctx { args=newArgs }
  where newArgs = Map.insert name value (args ctx)

interpretFnBody :: Context -> [Statement] -> TL Value
interpretFnBody []     = exitSuccess
interpretFnBody (s:ss) = case s of
  Return Nothing ->
    return EmptyValue
  Return (Just expr) ->
    evalExpr context expr
  Set name expr -> do
    value <- evalExpr context expr
    let context' = setLocal name value context
  
