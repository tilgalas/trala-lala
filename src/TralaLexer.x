{
module TralaLexer where

import TralaParserInternal
}

%wrapper "basic"

$whiteExceptNl = $white # \n
$digit = [0-9]

:-

$whiteExceptNl         ;
$digit+                { IntegerLit . read }
$digit*\.$digit+       { FloatLit . read }
[_a-zA-Z][_a-zA-Z0-9]* { Id }
=                      { const EqualSign }
==                     { const DblEqualSign }
::                     { const DblColon }
[\-\+\*\/\>\<:]+       { Operator }
\(                     { const LeftParen }
\)                     { const RightParen }
true                   { const TrueLit }
false                  { const FalseLit }
_                      { const Underscore }
\\^\n                  ;
\n                     { const Newline }
