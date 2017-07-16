import Control.Monad.Free

import IR
import Interpreter
import TopLevel (FO, runIO, writeString, exitStatus, updateGlobal, Context)

main :: IO ()
main = do
  success <- runIO fibContext fibExample
  putStrLn $ show success

example :: FO a b
example = do
  writeString "hello world\n"
  exitStatus 0

example2 :: FO Value Value
example2 =
  let fnExpr = GVar "print"
      argExprs = [
        Binary Plus (Val $ FloatVal 123) (Val $ FloatVal 456),
        Val $ StrVal " hi from interpreter\n"
        ]
      expr = Call fnExpr argExprs
  in evalExpr expr

fibExample :: FO Value Value
fibExample =
  let fibCall = Call (GVar "fib") [intVal 35]
  in evalExpr $ Call (GVar "print") [fibCall, Val $ StrVal "\n"]

fibContext :: Context Value
fibContext =
  let
    testExpr = Binary Less (Arg 0) (intVal 2)
    ifCase = Block [Return $ Just $ intVal 1]
    callExpr x = Call (GVar "fib") [Binary Minus (Arg 0) (intVal x)]
    recExpr = Binary Plus (callExpr 1) (callExpr 2)
    elseCase = Block [Return $ Just recExpr]
    branch = If testExpr ifCase (Just elseCase)
    fibFn = LambdaVal ["n"] branch
  in updateGlobal mainContext "fib" fibFn

intVal :: Int -> Expression
intVal = Val . IntVal
