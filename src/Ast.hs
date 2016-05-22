module Ast where

import Data.IORef (IORef, readIORef)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as M

type Name = String

newtype Fn = Fn ([SExpr] -> IO SExpr)

data EnvData = EnvPair (Maybe Env, (M.Map String SExpr))
type Env = IORef EnvData

data SExpr = ENil
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
           | MalFunc {fn :: Fn,
                      ast :: SExpr,
                      env :: Env,
                      params :: SExpr,
                      macro :: Bool,
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
stringOfExpr _     (MalFunc {ast=ast, env=fn_env, params=params}) = "(fn* " ++ (show params) ++ " " ++ (show ast) ++ ")"

instance Show SExpr where show = stringOfExpr True

mkFunc fn = Func (Fn fn) ENil
mkfuncWithMeta fn meta = Func (Fn fn) meta
