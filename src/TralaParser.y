{
module TralaParser (
    parse,
    Token (..),
    evalState,
    runExceptT,
    either,
    ParserState (..),
    ParserException (..)
) where


import TralaParserInternal
import Control.Monad.State
import Control.Monad.Except

}

%name parse
%tokentype { Token }
%error { parseError }
%monad { TralaParserMonad }
%lexer { lexer } { EOFToken }

%token
    token { NonWhiteToken _ }

%%

tokenList  : token           { [$1] }
           | tokenList token { $2 : $1 }
