module Prologue where

import Ast
import Scope
import Parser (parseExpr)
import System.IO (hFlush, stdout)
import qualified Data.Map as M

numOp op [ENum a, ENum b] = return $ ENum $ op a b
numOp _ _ = error "illegal arguments to number operation"

cmpOp op [ENum a, ENum b] = return $ if op a b then (EBool True) else (EBool False)
cmpOp _ _ = error "illegal arguments to comparison operation"

prStr args = return $ EString $ stringOfList True " " args

str args = return $ EString $ stringOfList False "" args

prn args = do
  putStrLn $ stringOfList True " " args
  hFlush stdout
  return ENil

println args = do
  putStrLn $ stringOfList False " " args
  hFlush stdout
  return ENil

slurp ([EString path]) = do
  str <- readFile path
  return $ EString str
slurp _ = error "invalid arguments to slurp"

builtins = [("+", mkFunc $ numOp (+)),
            ("-", mkFunc $ numOp (-)),
            ("*", mkFunc $ numOp (*)),
            ("/", mkFunc $ numOp (div)),
            ("<", mkFunc $ cmpOp (<)),
            ("≤", mkFunc $ cmpOp (<=)),
            (">", mkFunc $ cmpOp (>)),
            ("≥", mkFunc $ cmpOp (>=)),
            ("pr-str", mkFunc prStr),
            ("str", mkFunc str),
            ("prn", mkFunc prn),
            ("println", mkFunc println),
            ("read-string", mkFunc (\[(EString s)] -> return $ parseExpr s)),
            ("slurp", mkFunc slurp)]
