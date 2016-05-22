module Eval where

import Ast
import Control.Monad (mapM)
import Prelude hiding (lookup)
import qualified Data.Map as M
import qualified Data.Traversable as T

type Scope = M.Map Name SExpr

evalExpr :: SExpr -> Scope -> IO SExpr
evalExpr expr scope = case expr of
                        ESymbol name -> do
                          case M.lookup name scope of
                            Just v -> return v
                            Nothing -> error $ "symbol '" ++ name ++ "' not found"
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

applyExpr :: SExpr -> Scope -> IO SExpr
applyExpr expr scope = case expr of
                         EList [] _ -> return expr
                         EList _ _ -> do
                           el <- evalExpr expr scope
                           case el of
                             EList ((Func (Fn f) _) : args) _ -> f $ args
                             _ -> error "should apply args to a function."

eval :: SExpr -> Scope -> IO SExpr
eval expr scope = do
  case expr of
    EList _ _ -> applyExpr expr scope
    _ -> evalExpr expr scope
