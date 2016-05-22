module Ast where

import Data.IORef (IORef)
import qualified Data.Map as M

type Name = String

newtype Fn = Fn ([SExpr] -> IO SExpr)

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

mkFunc fn = Func (Fn fn) ENil
mkfuncWithMeta fn meta = Func (Fn fn) meta
