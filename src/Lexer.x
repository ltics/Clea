{
module Lexer where
import Data.Char (toUpper)
}

%wrapper "basic"

$digit = [0-9]
$char = [a-zA-Z\!\#\$\%\&\|\*\+\-\/\:\<\=\>\?\@\^\_\~]
$dc = [$char $digit]
$eol = [\n]

tokens :-
       $eol                        ;
       $white+                     ;
       ";;".*                      ; --comments
       "["                         { \_ -> LBRACKET }
       "]"                         { \_ -> RBRACKET }
       "("                         { \_ -> LPAREN }
       ")"                         { \_ -> RPAREN }
       "{"                         { \_ -> LBRACE }
       "}"                         { \_ -> RBRACE }
       "'"                         { \_ -> QUOTE }
       "`"                         { \_ -> SYNTAXQUOTE }
       "~"                         { \_ -> UNQUOTE }
       "^"                         { \_ -> META }
       "@"                         { \_ -> DEREF }
       "~@"                        { \_ -> UNQUOTESLICE }
       "nil"                       { \_ -> NIL }
       \"[^\"]*\"                  { \s -> STRING ((tail . init) s) }
       $digit+                     { \s -> NUMBER (read s) }
       '-' $digit+                 { \s -> NUMBER (- (read s)) }
       "true" | "false"            { \s -> BOOLEAN (read ([toUpper (s!!0)] ++ tail s)) }
       ":" $char $dc*              { \s -> KEYWORD (tail s) }
       $char $dc*                  { \s -> SYMBOL s }
{
data Token = LBRACKET
           | RBRACKET
           | LPAREN
           | RPAREN
           | LBRACE
           | RBRACE
           | QUOTE
           | UNQUOTE
           | SYNTAXQUOTE
           | META
           | DEREF
           | UNQUOTESLICE
           | NIL
           | STRING String
           | KEYWORD String
           | NUMBER Int
           | BOOLEAN Bool
           | SYMBOL String
           deriving(Eq, Show)

scanTokens = alexScanTokens
}