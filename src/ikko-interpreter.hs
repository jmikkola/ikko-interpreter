import Control.Monad.Free

import Interpreter

main :: IO ()
main = do
  success <- evalTopLevelIO example
  putStrLn $ show success

example :: TL ()
example = do
  writeString "hello world\n"
  exitStatus 0
