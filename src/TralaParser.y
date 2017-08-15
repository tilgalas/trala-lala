{
module TralaParser where


import TralaParserInternal
import TralaParsingCommon
import Data.Functor.Identity
import Data.Text (Text)
  
}

%name parse
%tokentype { Token Text }
%error { parseError }
%monad { TralaParserMonad Identity }
%lexer { lexer } { EOFToken }

%token
    integer { IntegerLit _ }
    float   { FloatLit _ }
    identifier { Id _ }
    equalSign { EqualSign }
    dblEqualSign { DblEqualSign }
    dblColon { DblColon }
    operator { Operator _ }
    leftParen { LeftParen }
    rightParen { RightParen }
    trueLit { TrueLit }
    falseLit { FalseLit }
    underscore { Underscore }
    newline { Newline }

%%

equationList : equation { [$1] }
             | equationList newline equation { $3 : $1 }
             | equationList newline { $1 }

equation   : equationTermTree equalSign equationTermTree { Equation $1 $3 }

equationTermTree : equationTerm { [$1] } |
                   equationTermTree equationTerm { $2 : $1 }

equationTerm : token { TokenTerm $1 } |
               leftParen equationTermTree rightParen { NestedTerm $2 }

token      : integer { $1 }
           | float { $1 }
           | identifier { $1 }

