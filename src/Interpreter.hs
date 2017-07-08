-- Used for `deriving (Functor)` on the TopLevel type:
{-# LANGUAGE DeriveFunctor #-}
-- Used for the `forall a.` on the various boolean operation functions:
{-# LANGUAGE Rank2Types #-}
-- Used to get the function iShiftRL# and the types it works with
{-# LANGUAGE MagicHash #-}

module Interpreter where

import GHC.Base (iShiftRL#, Int(I#))

import Control.Monad.Free
import Data.Bits (complement, (.&.), (.|.), xor, shiftL, shiftR)
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
evalExpr ctx expr = case expr of
  Paren inner ->
    evalExpr ctx inner
  Val value ->
    return value
  Unary op inner -> do
    innerVal <- evalExpr ctx inner
    applyUnary op innerVal
  Binary op left right -> do
    leftVal <- evalExpr ctx left
    rightVal <- evalExpr ctx right
    applyBinary op leftVal rightVal
  Call fnExpr argExprs -> do
    fnVal <- evalExpr ctx fnExpr
    argVals <- mapM (evalExpr ctx) argExprs
    callFunction fnVal argVals
  Var name ->
    lookupVar ctx name
  Arg num ->
    lookupArg ctx num

applyUnary :: UnaryOp -> Value -> TL Value
applyUnary BitInvert value = case value of
  IntVal i -> return (IntVal $ complement i)
  _        -> exitError $ "cannot bit invert " ++ show value
applyUnary BoolNot value = case value of
  BoolVal b -> return (BoolVal $ not b)
  _         -> exitError $ "cannot boolean not " ++ show value

applyBinary :: BinOp -> Value -> Value -> TL Value
applyBinary op left right =
  let fn = case op of
            Plus      -> applyNumOp (+)
            Minus     -> applyNumOp (-)
            Times     -> applyNumOp (*)
            Divide    -> applyNumFNs div (/)
            Mod       -> applyIntFn mod
            Power     -> applyNumFNs (^) (**)
            BitAnd    -> applyIntFn (.&.)
            BitOr     -> applyIntFn (.|.)
            BitXor    -> applyIntFn xor
            BoolAnd   -> applyBoolFn (&&)
            BoolOr    -> applyBoolFn (||)
            Eq        -> testEq True
            NotEq     -> testEq False
            Less      -> applyComparison (<)
            LessEq    -> applyComparison (<=)
            Greater   -> applyComparison (>)
            GreaterEq -> applyComparison (>=)
            LShift    -> applyIntFn shiftL
            RShift    -> applyIntFn shiftR
            RRShift   -> applyIntFn shiftRR
  in fn left right

shiftRR :: Int -> Int -> Int
shiftRR (I# n) bits@(I# b) =
  if bits < 0
  then error "TODO: handle negative right shift"
  else I# (iShiftRL# n b)

testEq :: Bool -> Value -> Value -> TL Value
--testEq eq (LambdaVal _) _             = return (BoolVal False)
--testEq eq _             (LambdaVal _) = return (BoolVal False)
testEq eq left          right         = return (BoolVal $ (left == right) == eq)

applyBoolFn :: (Bool -> Bool -> Bool)
               -> Value -> Value -> TL Value
applyBoolFn fn (BoolVal l) (BoolVal r) = return (BoolVal $ fn l r)
applyBoolFn _  left        right       =
  exitError $ "both must be booleans: " ++ show left ++ " and " ++ show right

applyIntFn :: (Int -> Int -> Int)
              -> Value -> Value -> TL Value
applyIntFn fn (IntVal l) (IntVal r) = return (IntVal $ fn l r)
applyIntFn _  left       right      =
  exitError $ "both must be integers: " ++ show left ++ " and " ++ show right

applyNumOp :: (forall a. (Num a) => a -> a -> a)
              -> Value -> Value -> TL Value
applyNumOp fn left right = applyNumFNs fn fn left right

applyNumFNs :: (Int -> Int -> Int) -> (Float -> Float -> Float)
               -> Value -> Value -> TL Value
applyNumFNs ifn _   (IntVal l) (IntVal r) =
  return $ IntVal   (ifn l r)
applyNumFNs _   ffn (FloatVal l) (FloatVal r) =
  return $ FloatVal (ffn l r)
applyNumFNs _ _ l r =
  exitError $ "cannot apply a numeric operation to " ++ show l ++ " and " ++ show r

applyComparison :: (forall a. (Ord a) => a -> a -> Bool)
                   -> Value -> Value -> TL Value
applyComparison fn (IntVal l) (IntVal r) =
  return $ BoolVal (fn l r)
applyComparison fn (FloatVal l) (FloatVal r) =
  return $ BoolVal (fn l r)
applyComparison fn (BoolVal l) (BoolVal r) =
  return $ BoolVal (fn l r)
applyComparison fn (StrVal l) (StrVal r) =
  return $ BoolVal (fn l r)
applyComparison _  left right =
  exitError $ "cannot compare " ++ show left ++ " and " ++ show right

callFunction :: Value -> [Value] -> TL Value
callFunction = undefined

lookupVar :: Context -> String -> TL Value
lookupVar = undefined

lookupArg :: Context -> Int -> TL Value
lookupArg = undefined

asBlock :: Statement -> [Statement]
asBlock (Block stmts) = stmts
asBlock stmt          = [stmt]
