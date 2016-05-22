{
module Parser where

import Ast
import Lexer
import qualified Data.Map as M
}

%name expr
%tokentype { Token }
%error { parseError }

%token
    '['                         { LBRACKET }
    ']'                         { RBRACKET }
    '('                         { LPAREN }
    ')'                         { RPAREN }
    '{'                         { LBRACE }
    '}'                         { RBRACE }
    nil                         { NIL }
    quote                       { QUOTE }
    syntaxquote                 { SYNTAXQUOTE }
    meta                        { META }
    '~'                         { UNQUOTE }
    '@'                         { DEREF }
    '~@'                        { UNQUOTESLICE }
    string                      { STRING $$ }
    number                      { NUMBER $$ }
    boolean                     { BOOLEAN $$ }
    keyword                     { KEYWORD $$ }
    symbol                      { SYMBOL $$ }

%%

Form : Macro                      { $1 }
     | List                       { $1 }
     | Vector                     { $1 }
     | Map                        { $1 }
     | Atom                       { $1 }

Forms : Form                      { [$1] }
      | Form Forms                { $1 : $2 }

Macro : quote Form                { EList [ESymbol "quote", $2] ENil }
      | syntaxquote Form          { EList [ESymbol "syntaxquote", $2] ENil }
      | '~' Form                  { EList [ESymbol "unquote", $2] ENil }
      | '~@' Form                 { EList [ESymbol "unquote-slice", $2] ENil }
      | '@' Form                  { EList [ESymbol "deref", $2] ENil }
      | meta Form Form            { EList [ESymbol "with-meta", $3, $2] ENil }

List : '(' Forms ')'              { EList $2 ENil }

Vector : '[' Forms ']'            { EVector $2 ENil }

Map : '{' Forms '}'               { EMap (M.fromList $ mkPairs $2) ENil }

Atom : number                     { ENum $1 }
     | boolean                    { EBool $1 }
     | string                     { EString $1 }
     | symbol                     { ESymbol $1 }
     | keyword                    { EKeyword $1 }
     | nil                        { ENil }

{
mkPairs [] = []
mkPairs [_] = error "Odd number of elements to pairs"
mkPairs ((EString k):v:t) = (k, v) : (mkPairs t)

parseError :: [Token] -> a
parseError _ = error "Parse error"

parseExpr :: String -> SExpr
parseExpr = expr . scanTokens
}