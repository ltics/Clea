module Prologue where

import Ast
import Scope
import Parser (parseExpr)
import System.IO (hFlush, stdout)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map as M

isEqual [a, b] = return $ if a == b then trueV else falseV
isEqual _ = error "illegal arguments to ="

run1 :: (SExpr -> SExpr) -> [SExpr] -> IO SExpr
run1 f (x:[]) = return $ f x
run1 _ _ = error "function takes a single argument"

run2 :: (SExpr -> SExpr -> SExpr) -> [SExpr] -> IO SExpr
run2 f (x:y:[]) = return $ f x y
run2 _ _ = error "function takes a two arguments"

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

atom (val:[]) = do
  ref <- newIORef val
  return $ EAtom ref ENil
atom _ = error "invalid atom call"

deref ((EAtom ref _):[]) = do
  val <- readIORef ref
  return val
deref _ = error "invalid deref call"

reset ((EAtom ref _):val:[]) = do
  writeIORef ref $ val
  return val
reset _ = error "invalid deref call"

swap ((EAtom ref _):args) = do
  val <- readIORef ref
  f <- getFn args
  new_val <- f $ [val] ++ (tail args)
  _ <- writeIORef ref $ new_val
  return new_val

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
            ("slurp", mkFunc slurp),
            ("atom", mkFunc $ atom),
            ("atom?", mkFunc $ run1 isAtom),
            ("deref", mkFunc $ deref),
            ("reset!", mkFunc $ reset),
            ("swap!", mkFunc $ swap)]
