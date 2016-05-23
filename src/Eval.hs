module Eval where

import Ast
import Scope
import Control.Monad (mapM, foldM)
import Prelude hiding (lookup)
import qualified Data.Map as M
import qualified Data.Traversable as T

syntaxquote :: SExpr -> SExpr
syntaxquote expr =
  case expr of
    EList ((ESymbol "unquote"):expr:[]) _ -> expr -- unquote a expr and try to eval it
    EList ((EList ((ESymbol "unquote-slice"):expr:[]) _):rest) _ -> EList [(ESymbol "concat"), expr, syntaxquote (EList rest ENil)] ENil
    EVector ((EList ((ESymbol "unquote-slice"):expr:[])_):rest) _ -> EList [(ESymbol "concat"), expr, syntaxquote (EVector rest ENil)] ENil
    EList (expr:rest) _ -> EList [(ESymbol "cons"), syntaxquote expr, syntaxquote (EList rest ENil)] ENil
    EVector (expr:rest) _ -> EList [(ESymbol "cons"), syntaxquote expr, syntaxquote (EVector rest ENil)] ENil
    _ -> EList [(ESymbol "quote"), expr] ENil -- just quote it

evalExpr :: SExpr -> Env -> IO SExpr
evalExpr expr scope = case expr of
                        ESymbol _ -> lookupValue scope expr
                        EList exprs nil -> do
                          newExprs <- mapM (\e -> eval e scope) exprs
                          return $ EList newExprs nil
                        EVector exprs nil -> do
                          newExprs <- mapM (\e -> eval e scope) exprs
                          return $ EVector newExprs nil
                        EMap pairs nil -> do
                          newPairs <- T.mapM (\e -> eval e scope) pairs
                          return $ EMap newPairs nil
                        _ -> return expr

letBind :: Env -> [SExpr] -> IO Env
letBind env [] = return env
letBind env (b:e:xs) = do
  evaled <- eval e env
  x <- insertValue env b evaled
  letBind env xs

applyExpr :: SExpr -> Env -> IO SExpr
applyExpr expr scope = case expr of
                         EList [] _ -> return expr
                         EList (ESymbol "def":args) _ -> do
                           case args of
                             (expr@(ESymbol _):a2:[]) -> do
                               a2V <- eval a2 scope
                               insertValue scope expr a2V
                             _ -> error "invalid def!"
                         EList (ESymbol "let*":args) _ -> do
                           case args of
                             (expr:a2:[]) -> do
                               params <- mkList expr
                               letScope <- createScope $ Just scope
                               -- evaluate k v pair and insert to current scope
                               letBind letScope params
                               eval a2 letScope
                             _ -> error "invalid let*"
                         EList (ESymbol "do":args) _ -> do
                           case args of
                             [] -> return ENil
                             _ -> do
                               el <- evalExpr (EList args ENil) scope
                               case el of
                                 (EList exprs _) -> return $ last exprs
                         EList (ESymbol "if":args) _ -> do
                           case args of
                             (cond:conse:alt:[]) -> do
                               condV <- eval cond scope
                               if condV == (EBool False) || condV == ENil
                               then eval alt scope
                               else eval conse scope
                             (cond:conse:[]) -> do
                               condV <- eval cond scope
                               if condV == (EBool False) || condV == ENil
                               then return ENil
                               else eval conse scope
                             _ -> error "invalid if"
                         EList (ESymbol "λ":args) _ -> do
                           case args of
                             (params:body:[]) -> do
                               params <- mkList params
                               return $ mkTcoFunc body scope (EList params ENil)
                                                  (\args -> do
                                                    initEnv <- createScope $ Just scope
                                                    bindEnv <- bindScope initEnv params args
                                                    eval body bindEnv)
                             _ -> error "invalid fn*"
                         EList (ESymbol "quote":args) _ -> do
                           case args of
                             expr:[] -> return expr -- return expr without eval
                             _ -> error "invalid quote"
                         EList (ESymbol "syntaxquote":args) _ -> do
                           case args of
                             expr : [] -> eval (syntaxquote expr) scope
                             _ -> error "invalid syntaxquote"
                         EList (EKeyword k:expr:[]) _ -> do
                           exprV <- eval expr scope
                           case exprV of
                             EMap hm _ -> case M.lookup k hm of
                                           Just mv -> return mv
                                           Nothing -> return ENil
                             _ -> do
                               putStrLn $ show exprV
                               return ENil
                         EList _ _ -> do
                           el <- evalExpr expr scope
                           case el of
                             EList ((Func (Fn f) _):args) _ -> f $ args
                             EList ((TcoFunc {ast = body, env = fnEnv, params = (EList params ENil)}):args) _ -> do
                               initEnv <- createScope $ Just fnEnv
                               bindEnv <- bindScope initEnv params args
                               eval body bindEnv
                             _ -> error "should apply args to a function."

eval :: SExpr -> Env -> IO SExpr
eval expr scope = do
  case expr of
    EProgram instrs -> foldM (\_ instr -> eval instr scope) ENil instrs
    EList _ _ -> applyExpr expr scope
    _ -> evalExpr expr scope
