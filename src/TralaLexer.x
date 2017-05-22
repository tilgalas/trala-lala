{
module TralaLexer where

import TralaParserInternal
}

%wrapper "basic"

$nonwhite = ~$white

:-

$white    ;
$nonwhite+    { NonWhiteToken }
