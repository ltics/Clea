module Scope where

import Ast
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (elemIndex)
import qualified Data.Map as M

-- cause use mutable environment so we should introduce a parent scope to hold for outer world

createScope :: Maybe Env -> IO Env
createScope outer = newIORef $ EnvPair (outer, (M.fromList []))

emptyScope = createScope Nothing

bindScope :: Env -> [SExpr] -> [SExpr] -> IO Env
bindScope envRef binds exprs = do
  case (elemIndex (ESymbol "&") binds) of
    Nothing -> do
        -- bind binds to exprs
        _ <- mapM (\(b,e) -> insertValue envRef b e) $ zip binds exprs
        return envRef
    Just idx -> do
        -- Varargs binding
        _ <- mapM (\(b,e) -> insertValue envRef b e) $
            zip (take idx binds) (take idx exprs)
        _ <- insertValue envRef (binds !! (idx + 1))
                            (EList (drop idx exprs) ENil)
        return envRef

lookupScope :: Env -> SExpr -> IO (Maybe Env)
lookupScope envRef sym@(ESymbol key) = do
  e <- readIORef envRef
  case e of
    EnvPair (o, m) -> case M.lookup key m of
                       Nothing -> case o of
                                   Nothing -> return Nothing
                                   Just o -> lookupScope o sym
                       Just val -> return $ Just envRef

lookupValue :: Env -> SExpr -> IO SExpr
lookupValue envRef sym@(ESymbol key) = do
  e1 <- lookupScope envRef sym
  case e1 of
    Nothing -> error $ "'" ++ key ++ "' not found"
    Just eRef -> do
      e2 <- readIORef eRef
      case e2 of
        EnvPair (o, m) -> case M.lookup key m of
                           Nothing -> error $ "lookupValue error"
                           Just val -> return val

insertValue :: Env -> SExpr -> SExpr -> IO SExpr
insertValue envRef (ESymbol key) val = do
  e <- readIORef envRef
  case e of
    EnvPair (o, m) -> writeIORef envRef $ EnvPair (o, (M.insert key val m))
  return val
