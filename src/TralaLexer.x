{
module TralaLexer where

import TralaAlexWrapper
import TralaParsingCommon

}

$whiteExceptNl = $white # \n
$digit = 0-9

:-

$whiteExceptNl         ;
$digit+                { readIntegerLit }
$digit*\.$digit+       { readFloatLit }
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
\.                     { const Dot }
\\^\n                  ;
\n                     { const Newline }
