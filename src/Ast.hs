module Ast where

import Data.List (intercalate)
import Data.IORef (IORef, readIORef)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as M

type Name = String

newtype Fn = Fn ([SExpr] -> IO SExpr)

data EnvData = EnvPair (Maybe Env, (M.Map String SExpr))
type Env = IORef EnvData

-- syntax is also value

-- last expr is just a placeholder for metadata
data SExpr = ENil
           | EProgram [SExpr]
           | EBool Bool
           | ENum Int
           | EString String
           | EKeyword String
           | ESymbol String
           | EList [SExpr] SExpr
           | EVector [SExpr] SExpr
           | EMap (M.Map String SExpr) SExpr
           | EAtom (IORef SExpr) SExpr
           | Func Fn SExpr
           | TcoFunc {fn :: Fn,
                      ast :: SExpr,
                      env :: Env,
                      params :: SExpr,
                      ismacro :: Bool,
                      meta :: SExpr}

stringOfList :: Bool -> String -> [SExpr] -> String
stringOfList pr sep [] = []
stringOfList pr sep (x:[]) = (stringOfExpr pr x)
stringOfList pr sep (x:xs) = (stringOfExpr pr x) ++ sep ++ (stringOfList pr sep xs)

flatTuples ((a,b):xs) = EString a : b : flatTuples xs
flatTuples _          = []

unescape chr = case chr of
    '\n' -> "\\n"
    '\\' -> "\\\\"
    '"'  -> "\\\""
    c    -> [c]

stringOfExpr :: Bool -> SExpr -> String
stringOfExpr _     (EProgram instrs) = intercalate "" $ map (\instr -> show instr ++ "\n") instrs
stringOfExpr _     (EKeyword key) = ":" ++ key
stringOfExpr True  (EString str) = "\"" ++ concatMap unescape str ++ "\""
stringOfExpr False (EString str) = str
stringOfExpr _     (ESymbol name) = name
stringOfExpr _     (ENum num) = show num
stringOfExpr _     (EBool bool) = show bool
stringOfExpr _     (ENil) = "nil"
stringOfExpr pr    (EList items _) = "(" ++ (stringOfList pr " " items) ++ ")"
stringOfExpr pr    (EVector items _) = "[" ++ (stringOfList pr " " items) ++ "]"
stringOfExpr pr    (EMap m _) = "{" ++ (stringOfList pr " " (flatTuples $ M.assocs m)) ++ "}"
stringOfExpr pr    (EAtom r _) = "(atom " ++ (stringOfExpr pr (unsafePerformIO (readIORef r))) ++ ")"
stringOfExpr _     (Func f _) = "<fun>"
stringOfExpr _     (TcoFunc {ast = ast, env = fn_env, params = params}) = "(λ " ++ (show params) ++ " " ++ (show ast) ++ ")"

instance Show SExpr where show = stringOfExpr True

instance Eq SExpr where
  ENil == ENil = True
  (EBool a) == (EBool b) = a == b
  (ENum a) == (ENum b) = a == b
  (EString a) == (EString b) = a == b
  (ESymbol a) == (ESymbol b) = a == b
  (EList a _) == (EList b _) = a == b
  (EList a _) == (EVector b _) = a == b
  (EVector a _) == (EList b _) = a == b
  (EVector a _) == (EVector b _) = a == b
  (EMap a _) == (EMap b _) = a == b
  (EAtom a _) == (EAtom b _) = a == b
  _ == _ = False

getFn ((Func (Fn f) _):_) = return f
getFn (TcoFunc {fn = (Fn f)}:_) = return f
getFn _ = error "getFn first parameter is not a function "

toList (EList lst _) = return lst
toList (EVector lst _) = return lst
toList _ = error "toList expected a EList or EVector"

mkFunc fn = Func (Fn fn) ENil
mkfuncWithMeta fn meta = Func (Fn fn) meta

mkTcoFunc ast env params fn = TcoFunc {fn = (Fn fn),
                                       ast = ast,
                                       env = env,
                                       params = params,
                                       ismacro = False,
                                       meta = ENil}
mkTcoFuncWithMeta ast env params fn meta = TcoFunc {fn = (Fn fn),
                                                    ast = ast,
                                                    env = env,
                                                    params = params,
                                                    ismacro = False,
                                                    meta = meta}

mkList (EList lst _) = return lst
mkList (EVector lst _) = return lst
mkList _ = error "mkList expected a List or Vector"

trueV = EBool True
falseV = EBool False

isNil ENil = trueV
isNil _ = falseV

isTrue (EBool True) = trueV
isTrue _ = falseV

isFalse (EBool False) = trueV
isFalse _ = falseV

isSymbol (ESymbol _) = trueV
isSymbol _ = falseV

isNumber (ENum _) = trueV
isNumber _ = falseV

isString (EString _) = trueV
isString _ = falseV

isKeyword (ESymbol _) = trueV
isKeyword _ = falseV

isList (EList _ _) = trueV
isList _ = falseV

isVector (EVector _ _) = trueV
isVector _ = falseV

isMap (EMap _ _) = trueV
isMap _ = falseV

isAtom (EAtom _ _) = trueV
isAtom _ = falseV
