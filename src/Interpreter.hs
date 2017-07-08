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
setLocal name value ctx = ctx { locals=newLocals }
  where newLocals = Map.insert name value (locals ctx)

data BlockResult
  = BlockReturn Value
  | BlockEnd
  deriving (Show)

interpretFnBody :: Context -> [Statement] -> TL Value
interpretFnBody ctx statements = do
  blockResult <- interpretBlock ctx statements
  case blockResult of
   BlockEnd      -> return EmptyValue
   BlockReturn v -> return v

interpretBlock :: Context -> [Statement] -> TL BlockResult
interpretBlock ctx []     = return BlockEnd
interpretBlock ctx (s:ss) = case s of
  Return Nothing ->
    return $ BlockReturn EmptyValue
  Return (Just expr) -> do
    val <- evalExpr ctx expr
    return $ BlockReturn val
  Set name expr -> do
    value <- evalExpr ctx expr
    let ctx' = setLocal name value ctx
    interpretBlock ctx' ss
  Block stmts -> do
    -- TODO: Pass variables back up from the block
    innerBlockResult <- interpretBlock ctx stmts
    case innerBlockResult of
     BlockReturn _ -> return innerBlockResult
     BlockEnd      -> interpretBlock ctx ss
  Expr expr -> do
    _ <- evalExpr ctx expr
    interpretBlock ctx ss
  If test thenCase elseCase -> do
    testResult <- evalExpr ctx test
    innerBlockResult <-
      if testResult == BoolVal True
      then interpretBlock ctx (asBlock thenCase)
      else case elseCase of
            Nothing   -> return BlockEnd
            Just stmt -> interpretBlock ctx (asBlock stmt)
    case innerBlockResult of
     BlockReturn _ -> return innerBlockResult
     BlockEnd      -> interpretBlock ctx ss
  While test body -> do
    testResult <- evalExpr ctx test
    if testResult == BoolVal True
      then do
      innerBlockResult <- interpretBlock ctx (asBlock body)
      case innerBlockResult of
       BlockReturn _ -> return innerBlockResult
       BlockEnd      -> interpretBlock ctx (s:ss)
      else interpretBlock ctx ss

evalExpr :: Context -> Expression -> TL Value
evalExpr = undefined

asBlock :: Statement -> [Statement]
asBlock (Block stmts) = stmts
asBlock stmt          = [stmt]
