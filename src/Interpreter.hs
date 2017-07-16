-- Used for the `forall a.` on the various boolean operation functions:
{-# LANGUAGE Rank2Types #-}
-- Used to get the function iShiftRL# and the types it works with
{-# LANGUAGE MagicHash #-}

module Interpreter where

import GHC.Base (iShiftRL#, Int(I#))

import Data.Bits (complement, (.&.), (.|.), xor, shiftL, shiftR)
import Data.Map
import qualified Data.Map as Map

import IR
import TopLevel


mainContext :: Context Value
mainContext = Context { args=[], globals=defaultGlobals, locals=Map.empty }

defaultGlobals :: Map String Value
defaultGlobals =
  Map.fromList
  [builtIn "print" printBuiltin]

builtIn :: String -> TFunc Value Value -> (String, Value)
builtIn name fn = (name, BuiltIn name $ BuiltInFn fn)

printBuiltin :: TFunc Value Value
printBuiltin argVals = do
  _ <- mapM (writeString . display) argVals
  return EmptyValue


data BlockResult
  = BlockReturn Value
  | BlockEnd
  deriving (Show)

interpretFnBody :: [Statement] -> FO Value Value
interpretFnBody statements = do
  blockResult <- interpretBlock statements
  case blockResult of
   BlockEnd      -> return EmptyValue
   BlockReturn v -> return v

interpretBlock :: [Statement] -> FO Value BlockResult
interpretBlock []     = return BlockEnd
interpretBlock (s:ss) = case s of
  Return Nothing ->
    return $ BlockReturn EmptyValue
  Return (Just expr) -> do
    val <- evalExpr expr
    return $ BlockReturn val
  Set name expr -> do
    value <- evalExpr expr
    putVar name value
    interpretBlock ss
  Block stmts -> do
    -- TODO: Pass variables back up from the block
    innerBlockResult <- interpretBlock stmts
    case innerBlockResult of
     BlockReturn _ -> return innerBlockResult
     BlockEnd      -> interpretBlock ss
  Expr expr -> do
    _ <- evalExpr expr
    interpretBlock ss
  If test thenCase elseCase -> do
    testResult <- evalExpr test
    innerBlockResult <-
      if testResult == BoolVal True
      then interpretBlock (asBlock thenCase)
      else case elseCase of
            Nothing   -> return BlockEnd
            Just stmt -> interpretBlock (asBlock stmt)
    case innerBlockResult of
     BlockReturn _ -> return innerBlockResult
     BlockEnd      -> interpretBlock ss
  While test body -> do
    testResult <- evalExpr test
    if testResult == BoolVal True
      then do
      innerBlockResult <- interpretBlock (asBlock body)
      case innerBlockResult of
       BlockReturn _ -> return innerBlockResult
       BlockEnd      -> interpretBlock (s:ss)
      else interpretBlock ss

evalExpr :: Expression -> FO Value Value
evalExpr expr = case expr of
  Paren inner ->
    evalExpr inner
  Val value ->
    return value
  Unary op inner -> do
    innerVal <- evalExpr inner
    applyUnary op innerVal
  Binary op left right -> do
    leftVal <- evalExpr left
    rightVal <- evalExpr right
    applyBinary op leftVal rightVal
  Call fnExpr argExprs -> do
    fnVal <- evalExpr fnExpr
    argVals <- mapM evalExpr argExprs
    callFunction fnVal argVals
  Var name ->
    getVar name
  GVar name ->
    getGlobal name
  Arg num ->
    getArg num

applyUnary :: UnaryOp -> Value -> FO Value Value
applyUnary BitInvert value = case value of
  IntVal i -> return (IntVal $ complement i)
  _        -> exitError $ "cannot bit invert " ++ show value
applyUnary BoolNot value = case value of
  BoolVal b -> return (BoolVal $ not b)
  _         -> exitError $ "cannot boolean not " ++ show value

applyBinary :: BinOp -> Value -> Value -> FO Value Value
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

testEq :: Bool -> Value -> Value -> FO Value Value
testEq eq (LambdaVal _ _) _             = return (BoolVal False)
testEq eq _               (LambdaVal _ _) = return (BoolVal False)
testEq eq left            right         = return (BoolVal $ (left == right) == eq)

applyBoolFn :: (Bool -> Bool -> Bool)
               -> Value -> Value -> FO Value Value
applyBoolFn fn (BoolVal l) (BoolVal r) = return (BoolVal $ fn l r)
applyBoolFn _  left        right       =
  exitError $ "both must be booleans: " ++ show left ++ " and " ++ show right

applyIntFn :: (Int -> Int -> Int)
              -> Value -> Value -> FO Value Value
applyIntFn fn (IntVal l) (IntVal r) = return (IntVal $ fn l r)
applyIntFn _  left       right      =
  exitError $ "both must be integers: " ++ show left ++ " and " ++ show right

applyNumOp :: (forall a. (Num a) => a -> a -> a)
              -> Value -> Value -> FO Value Value
applyNumOp fn left right = applyNumFNs fn fn left right

applyNumFNs :: (Int -> Int -> Int) -> (Float -> Float -> Float)
               -> Value -> Value -> FO Value Value
applyNumFNs ifn _   (IntVal l)   (IntVal r)   =
  return $ IntVal   (ifn l r)
applyNumFNs _   ffn (FloatVal l) (FloatVal r) =
  return $ FloatVal (ffn l r)
applyNumFNs _ _ l r =
  exitError $ "cannot apply a numeric operation to " ++ show l ++ " and " ++ show r

applyComparison :: (forall a. (Ord a) => a -> a -> Bool)
                   -> Value -> Value -> FO Value Value
applyComparison fn (IntVal l)   (IntVal r)   =
  return $ BoolVal (fn l r)
applyComparison fn (FloatVal l) (FloatVal r) =
  return $ BoolVal (fn l r)
applyComparison fn (BoolVal l)  (BoolVal r)  =
  return $ BoolVal (fn l r)
applyComparison fn (StrVal l)   (StrVal r)   =
  return $ BoolVal (fn l r)
applyComparison _  left         right        =
  exitError $ "cannot compare " ++ show left ++ " and " ++ show right

callFunction :: Value -> [Value] -> FO Value Value
callFunction (LambdaVal argNames body) argVals =
  let fn a = if length argNames /= length a
             then exitError "wrong number of arguments"
             else interpretFnBody (asBlock body)
  in callFn fn argVals
callFunction (BuiltIn _ (BuiltInFn f)) argVals =
  f argVals
callFunction nonFunction _ =
  exitError $ "cannot call " ++ show nonFunction ++ " as a function"

asBlock :: Statement -> [Statement]
asBlock (Block stmts) = stmts
asBlock stmt          = [stmt]
