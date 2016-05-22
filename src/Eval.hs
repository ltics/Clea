module Eval where

import Ast
import Scope
import Control.Monad (mapM)
import Prelude hiding (lookup)
import qualified Data.Map as M
import qualified Data.Traversable as T

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
                         EList (ESymbol "def!" : args) _ -> do
                           case args of
                             (a1@(ESymbol _) : a2 : []) -> do
                               a2V <- eval a2 scope
                               insertValue scope a1 a2V
                             _ -> error "invalid def!"
                         EList (ESymbol "let*" : args) _ -> do
                           case args of
                             (a1 : a2 : []) -> do
                               params <- mkList a1
                               letScope <- createScope $ Just scope
                               -- evaluate k v pair and insert to current scope
                               letBind letScope params
                               eval a2 letScope
                             _ -> error "invalid let*"
                         EList _ _ -> do
                           el <- evalExpr expr scope
                           case el of
                             EList ((Func (Fn f) _) : args) _ -> f $ args
                             _ -> error "should apply args to a function."

eval :: SExpr -> Env -> IO SExpr
eval expr scope = do
  case expr of
    EList _ _ -> applyExpr expr scope
    _ -> evalExpr expr scope
