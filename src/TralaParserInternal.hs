module TralaParserInternal where

import qualified Control.Monad.Except as Except
import qualified Control.Monad.State as State

type ParserMonad e s = Except.ExceptT e (State.State s)

data ParserState = ParserState [Token]
data ParserException = ParserException {
    getMessage :: String
}

type TralaParserMonad = ParserMonad ParserException ParserState

data Token = EOFToken | NonWhiteToken String
    deriving (Eq, Show)

parseError :: Token -> TralaParserMonad a
parseError _ = Except.throwError $ ParserException "parse error"

lexer :: (Token -> TralaParserMonad a) -> TralaParserMonad a
lexer c = do
    ParserState tokenList <- State.get
    case tokenList of
        [] -> c EOFToken
        tok:tokens -> do
            State.put $ ParserState tokens
            c tok
