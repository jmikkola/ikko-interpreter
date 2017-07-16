-- Used for `deriving (Functor)` on the TopLevel type:
{-# LANGUAGE DeriveFunctor #-}

module TopLevel where

import Control.Monad.Free
import Data.Map (Map)
import qualified Data.Map as Map

data Toplevel next
  = Exit Int
  | WriteString String next
  | Error String
  deriving (Functor)

type TL a = Free Toplevel a

ioExit :: Int -> TL a
ioExit status = liftFree (Exit status)

ioWrite :: String -> TL ()
ioWrite str = liftFree (WriteString str ())

ioErr :: String -> TL a
ioErr err = liftFree (Error err)

data FuncOp b next
  = FExit Int
  | FWrite String next
  | FError String
  | FGetVar String (b -> next)
  | FPutVar String b next
  | FCall (TFunc b b) [b] (b -> next)
  | FGetArg Int (b -> next)
  | FGetGlobal String (b -> next)
  | FPutGlobal String b next
  deriving (Functor)

type FO a p = Free (FuncOp a) p

type TFunc a b = [a] -> FO a b

exitStatus :: Int -> FO a p
exitStatus status = liftFree (FExit status)

exitSuccess :: FO a p
exitSuccess = exitStatus 0

exitError :: String -> FO a p
exitError err = liftFree (FError err)

writeString :: String -> FO a ()
writeString s = liftFree (FWrite s ())

getVar :: String -> FO a a
getVar name = liftFree (FGetVar name id)

putVar :: String -> a -> FO a ()
putVar name value = liftFree (FPutVar name value ())

callFn :: (TFunc a a) -> [a] -> FO a a
callFn fn args = liftFree (FCall fn args id)

getArg :: Int -> FO a a
getArg n = liftFree (FGetArg n id)

getGlobal :: String -> FO a a
getGlobal name = liftFree (FGetGlobal name id)

putGlobal :: String -> a -> FO a ()
putGlobal name value = liftFree (FPutGlobal name value ())

liftFree :: (Functor f) => f r -> Free f r
liftFree command = Free (fmap Pure command)

data Context a
  = Context
    { args :: [a]
    , globals :: Globals a
    , locals :: Map String a
    }

--mainContext :: Context a
--mainContext = Context {args=[], globals=Map.empty, locals=Map.empty}

type Globals a = Map String a

runIO :: Context a -> FO a b -> IO Bool
runIO ctx program = evalTopLevelIO $ runFuncOp ctx program

runFuncOp :: Context a -> FO a b -> TL (b, Globals a)
runFuncOp ctx (Pure a)  = Pure (a, globals ctx)
runFuncOp ctx (Free op) = case op of
  FExit i ->
    ioExit i
  FError err ->
    ioErr err
  FWrite s next -> do
    ioWrite s
    runFuncOp ctx next
  FGetVar name f -> do
    val <- lookupLocal ctx name
    runFuncOp ctx (f val)
  FPutVar name val next ->
    runFuncOp (updateLocal ctx name val) next
  FGetGlobal name f -> do
    val <- lookupGlobal ctx name
    runFuncOp ctx (f val)
  FPutGlobal name val next ->
    runFuncOp (updateGlobal ctx name val) next
  FGetArg i f -> do
    val <- lookupArg ctx i
    runFuncOp ctx (f val)
  FCall fn args f -> do
    (retVal, newGlobals) <- callFunc ctx fn args
    let ctx' = ctx {globals=newGlobals}
    runFuncOp ctx' (f retVal)

callFunc :: Context a -> TFunc a b -> [a] -> TL (b, Globals a)
callFunc ctx fn argVals = do
  let fnCtx = ctx { args=argVals, locals=Map.empty }
  (retVal, fnGlobals) <- runFuncOp fnCtx (fn argVals)
  return (retVal, fnGlobals)

lookupArg :: Context a -> Int -> TL a
lookupArg ctx n =
  let argVals = args ctx
      nArgs = length argVals
  in if n < 0 || n >= nArgs
     then ioErr $ "arg # out of bounds " ++ show n
     else return (argVals !! n)

lookupLocal :: Context a -> String -> TL a
lookupLocal ctx name = case Map.lookup name (locals ctx) of
  Nothing -> ioErr $ "variable not defined: " ++ name
  Just x  -> return x

updateLocal :: Context a -> String -> a -> Context a
updateLocal ctx name value =
  let newLocals = Map.insert name value (locals ctx)
  in ctx {locals=newLocals}

lookupGlobal :: Context a -> String -> TL a
lookupGlobal ctx name = case Map.lookup name (globals ctx) of
  Nothing -> ioErr $ "variable not defined: " ++ name
  Just x  -> return x

updateGlobal :: Context a -> String -> a -> Context a
updateGlobal ctx name value =
  let newGlobals = Map.insert name value (globals ctx)
  in ctx {globals=newGlobals}

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
