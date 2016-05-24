module Prologue where

import Ast
import Scope
import Debug.Trace
import Eval (isMacroCall)
import Parser (parseExpr)
import System.IO (hFlush, stdout)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map as M

isEqual [a, b] = return $ if a == b then trueV else falseV
isEqual _ = error "illegal arguments to ="

isNotEqual [a, b] = return $ if a /= b then trueV else falseV
isNotEqual _ = error "illegal arguments to ≠"

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
doConcat args = return $ EList (foldl concat1 [] args) ENil

nth ((EList lst _):(ENum idx):[]) = do
  if idx < length lst then return $ lst !! idx
  else error "nth: index out of range"
nth ((EVector lst _):(ENum idx):[]) = do
  if idx < length lst then return $ lst !! idx
  else error "nth: index out of range"
nth _ = error "invalid call to nth"

first ENil = ENil
first (EList lst _) = if length lst > 0 then lst !! 0 else ENil
first (EVector lst _) = if length lst > 0 then lst !! 0 else ENil

rest ENil = EList [] ENil
rest (EList lst _) = EList (drop 1 lst) ENil
rest (EVector lst _) = EList (drop 1 lst) ENil

isEmpty ENil = trueV
isEmpty (EList [] _) = trueV
isEmpty (EVector [] _) = trueV
isEmpty _ = falseV

count (ENil:[]) = return $ ENum 0
count (EList lst _:[]) = return $ ENum $ length lst
count (EVector lst _:[]) = return $ ENum $ length lst
count _ = error $ "non-sequence passed to count"

apply args = do
  f <- getFn args
  lst <- toList $ last args
  f lst

doMap args = do
  f <- getFn args
  lst <- toList (args !! 1)
  do new_lst <- mapM (\x -> f [x]) lst
     return $ EList new_lst ENil

conj ((EList lst _):args) = return $ EList ((reverse args) ++ lst) ENil
conj ((EVector lst _):args) = return $ EVector (lst ++ args) ENil
conj _ = error $ "illegal arguments to conj"

doSeq ((EList [] _):[]) = return $ ENil
doSeq (l@(EList _ _):[]) = return $ l
doSeq (EVector [] _:[]) = return $ ENil
doSeq (EVector lst _:[]) = return $ EList lst ENil
doSeq (EString []:[]) = return $ ENil
doSeq (EString s:[]) = return $ EList [EString [c] | c <- s] ENil
doSeq (ENil:[]) = return $ ENil
doSeq _ = error $ "seq: called on non-sequence"

mkPair [x] = error "Odd number of elements to mkPair"
mkPair [] = return []
mkPair ((EString x):y:xs) = do
  rest <- mkPair xs
  return $ (x, y):rest
mkPair ((EKeyword x):y:xs) = do
  rest <- mkPair xs
  return $ (x, y):rest

hashMap args = do
  pairs <- mkPair args
  return $ EMap (M.fromList pairs) ENil

assoc (EMap hm _:kvs) = do
  pairs <- mkPair kvs
  return $ EMap (M.union (M.fromList pairs) hm) ENil
assoc _ = error "invalid call to assoc"

dissoc (EMap hm _:ks) = do
  let remover = (\hm key -> case key of
                             EString k -> M.delete k hm
                             EKeyword k -> M.delete k hm) in
      return $ EMap (foldl remover hm ks) ENil
dissoc _ = error "invalid call to dissoc"

get (EMap hm _:EString k:[]) = do
  case M.lookup k hm of
    Just mv -> return mv
    Nothing -> return ENil
get (EMap hm _:EKeyword k:[]) = do
  case M.lookup k hm of
    Just mv -> return mv
    Nothing -> return ENil
get (ENil:EString k:[]) = return ENil
get (ENil:EKeyword k:[]) = return ENil
get _ = error "invalid call to get"

isContains (EMap hm _:EString k:[]) = do
  if M.member k hm then return trueV
  else return falseV
isContains (ENil:EString k:[]) = return falseV
isContains (EMap hm _:EKeyword k:[]) = do
  if M.member k hm then return trueV
  else return falseV
isContains (ENil:EKeyword k:[]) = return falseV
isContains _ = error "invalid call to contains?"

keys (EMap hm _:[]) = do
  return $ EList (map EKeyword (M.keys hm)) ENil
keys _ = error "invalid call to keys"

vals (EMap hm _:[]) = do
  return $ EList (M.elems hm) ENil
vals _ = error "invalid call to vals"

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

withMeta ((EList lst _):m:[]) = return $ EList lst m
withMeta ((EVector lst _):m:[]) = return $ EVector lst m
withMeta ((EMap hm _):m:[]) = return $ EMap hm m
withMeta ((EAtom atm _):m:[]) = return $ EAtom atm m
withMeta ((Func f _):m:[]) = return $ Func f m
withMeta ((TcoFunc {fn = f, ast = a, env = e, params = p, ismacro = mc}):m:[]) =
 return $ TcoFunc {fn = f, ast = a, env = e, params = p, ismacro = mc, meta = m}
withMeta _ = error $ "invalid with-meta call"

doMeta ((EList _ m):[]) = return m
doMeta ((EVector _ m):[]) = return m
doMeta ((EMap _ m):[]) = return m
doMeta ((EAtom _ m):[]) = return m
doMeta ((Func _ m):[]) = return m
doMeta ((TcoFunc {meta = m}):[]) = return m
doMeta _ = error $ "invalid meta call"

desugerStrV (EString msg) = msg
desugerStrV _ = error $ "not string value"

builtins = [("=", mkFunc isEqual),
            ("≠", mkFunc isNotEqual),
            ("+", mkFunc $ numOp (+)),
            ("-", mkFunc $ numOp (-)),
            ("*", mkFunc $ numOp (*)),
            ("/", mkFunc $ numOp (div)),
            ("%", mkFunc $ numOp (mod)),
            ("<", mkFunc $ cmpOp (<)),
            ("≤", mkFunc $ cmpOp (<=)),
            (">", mkFunc $ cmpOp (>)),
            ("≥", mkFunc $ cmpOp (>=)),
            ("nil?", mkFunc $ run1 $ isNil),
            ("true?", mkFunc $ run1 $ isTrue),
            ("false?", mkFunc $ run1 $ isFalse),
            ("string?", mkFunc $ run1 $ isString),
            ("int?", mkFunc $ run1 $ isNumber),
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
            ("concat", mkFunc $ doConcat),
            ("nth", mkFunc nth),
            ("first", mkFunc $ run1 $ first),
            ("rest", mkFunc $ run1 $ rest),
            ("empty?", mkFunc $ run1 $ isEmpty),
            ("count", mkFunc $ count),
            ("apply", mkFunc $ apply),
            ("map", mkFunc $ doMap),
            ("conj", mkFunc $ conj),
            ("seq", mkFunc $ doSeq),
            ("hash-map", mkFunc $ hashMap),
            ("map?", mkFunc $ run1 isMap),
            ("assoc", mkFunc $ assoc),
            ("dissoc", mkFunc $ dissoc),
            ("get", mkFunc $ get),
            ("contains?", mkFunc $ isContains),
            ("keys", mkFunc $ keys),
            ("vals", mkFunc $ vals),
            ("with-meta", mkFunc $ withMeta),
            ("meta", mkFunc $ doMeta),
            ("print", mkFunc $ run1 (\v -> trace (desugerStrV v) ENil)),
            ("error", mkFunc $ run1 (\v -> error $ desugerStrV v))]
