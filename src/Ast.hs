module Ast where

type Name = String

data SExpr = SExpr [SExpr]
           | Quote SExpr
           | UNQuote SExpr
           | UNQuoteSlice SExpr
           | Deref SExpr
           | Atomic Value

data Value = VStr String
           | VNum Int
           | Symbol Name
