{
module TralaLexer where

import TralaAlexWrapper
import TralaParsingCommon

}

$whiteExceptNl = $white # \n
$digit = 0-9
$operatorChar = [ \- \+ \* \/ \> \< : = \# @ ]
$quot = \"
$stringLitChar = [~$white \ ] # $quot # \\

:-

$whiteExceptNl         ;
$digit+                { readIntegerLit }
$digit* \. $digit+     { readFloatLit }
[_a-zA-Z][_a-zA-Z0-9]* { Id }
=                      { const EqualSign }
::                     { const DblColon }
$operatorChar+         { Operator }
\(                     { const LeftParen }
\)                     { const RightParen }
true                   { const TrueLit }
false                  { const FalseLit }
_                      { const Underscore }
\.                     { const Dot }
\,                     { const Comma }
\n                     { const Newline }
$quot ($stringLitChar|\\$printable)* $quot { readStringLit }
