import Control.Monad.Free

import IR
import Interpreter
import TopLevel (TL, evalTopLevelIO, writeString, exitStatus)

main :: IO ()
main = do
  success <- evalTopLevelIO example2
  putStrLn $ show success

example :: TL ()
example = do
  writeString "hello world\n"
  exitStatus 0

example2 :: TL Value
example2 =
  let fnExpr = GVar "print"
      argExprs = [
        Binary Plus (Val $ FloatVal 123) (Val $ FloatVal 456),
        Val $ StrVal "\nhi from interpreter\n"
        ]
      expr = Call fnExpr argExprs
  in evalExpr mainContext expr
