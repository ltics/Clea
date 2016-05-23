module Prologue where

import Ast
import Scope
import Parser (parseExpr)
import System.IO (hFlush, stdout)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map as M
import Prelude hiding (concat)

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

isSeq (EList _ _) = trueV
isSeq (EVector _ _) = trueV
isSeq _ = falseV

symbol (EString str:[]) = return $ ESymbol str
symbol _ = error "symbol called with non-string"

keyword (EString str:[]) = return $ EKeyword str
keyword _ = error "keyword called with non-string"

list args = return $ EList args ENil
vector args = return $ EVector args ENil

cons x ENil = EList [x] ENil
cons x (EList lst _) = EList (x:lst) ENil
cons x (EVector lst _) = EList (x:lst) ENil

concat1 a (EList lst _) = a ++ lst
concat1 a (EVector lst _) = a ++ lst
concat args = return $ EList (foldl concat1 [] args) ENil

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
            ("nil?", mkFunc $ run1 $ isNil),
            ("true?", mkFunc $ run1 $ isTrue),
            ("false?", mkFunc $ run1 $ isFalse),
            ("string?", mkFunc $ run1 $ isString),
            ("symbol", mkFunc $ symbol),
            ("symbol?", mkFunc $ run1 $ isSymbol),
            ("keyword", mkFunc $ keyword),
            ("keyword?", mkFunc $ run1 $ isKeyword),
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
            ("swap!", mkFunc $ swap),
            ("list", mkFunc $ list),
            ("list?", mkFunc $ run1 isList),
            ("vector", mkFunc $ vector),
            ("vector?", mkFunc $ run1 isVector),
            ("seq?", mkFunc $ run1 isSeq),
            ("cons", mkFunc $ run2 $ cons),
            ("concat", mkFunc $ concat)]
