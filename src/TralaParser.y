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
    dummy    { DummyToken }

%%

dummyRule : dummy { 1 } |
            dummyRule dummy { $1 + 1 }
