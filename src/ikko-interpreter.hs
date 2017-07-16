import Control.Monad.Free

import IR
import Interpreter
import TopLevel (FO, runIO, writeString, exitStatus)

main :: IO ()
main = do
  success <- runIO mainContext example2
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
