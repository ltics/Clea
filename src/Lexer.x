{
module Lexer where
}

%wrapper "basic"

$digit = [0-9]
$char = [a-zA-Z\_\+\*\/\=\<\>\&\|\?\!\.\-]
$dc = [$char $digit]
$eol = [\n]

tokens :-
       $eol                        ;
       $white+                     ;
       ";;".*                      ; --comments
       "("                         { \_ -> LPAREN }
       ")"                         { \_ -> RPAREN }
       "'"                         { \_ -> QUOTE }
       "~"                         { \_ -> UNQUOTE }
       "@"                         { \_ -> DEREF }
       "~@"                        { \_ -> UNQUOTESLICE }
       \"[^\"]*\"                  { \s -> STRING ((tail . init) s) }
       $digit+                     { \s -> NUMBER (read s) }
       $digit $dc*                 { \s -> SYMBOL s }
{
data Token = LPAREN
           | RPAREN
           | QUOTE
           | UNQUOTE
           | DEREF
           | UNQUOTESLICE
           | STRING String
           | NUMBER Int
           | SYMBOL String
           deriving(Eq, Show)

scanTokens = alexScanTokens
}