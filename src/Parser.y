{
module Parser where

import Ast
import Lexer
}

%name expr
%tokentype { Token }
%error { parseError }

%token
    '('                         { LPAREN }
    ')'                         { RPAREN }
    quote                       { QUOTE }
    '~'                         { UNQUOTE }
    '@'                         { DEREF }
    '~@'                        { UNQUOTESLICE }
    string                      { STRING $$ }
    number                      { NUMBER $$ }
    symbol                      { SYMBOL $$ }

%%

Expr : '(' Exprs ')'            { SExpr $2 }
     | quote Expr               { Quote $2 }
     | '~' Expr                 { UNQuote $2 }
     | '@' Expr                 { Deref $2 }
     | '~@' Expr                { UNQuoteSlice $2 }
     | Atom                     { Atomic $1 }

Exprs : Expr                    { [$1] }
      | Expr Exprs              { $1 : $2 }

Atom : string                   { VStr $1 }
     | number                   { VNum $1 }
     | symbol                   { Symbol $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

parseExpr :: String -> SExpr
parseExpr = expr . scanTokens
}