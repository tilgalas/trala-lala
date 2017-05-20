module TralaParserInternal where

import qualified Control.Monad.Except as Except
import qualified Control.Monad.State as State

type ParserMonad e s = Except.ExceptT e (State.State s)

data ParserState = ParserState Int
data ParserException = ParserException {
    getMessage :: String
}

type TralaParserMonad = ParserMonad ParserException ParserState

data Token = DummyToken | EOFToken

parseError :: Token -> TralaParserMonad a
parseError _ = Except.throwError $ ParserException "parse error"

lexer :: (Token -> TralaParserMonad a) -> TralaParserMonad a
lexer c = do
    ParserState i <- State.get
    if i == 0 then
        c EOFToken
    else if i < 0 then
        Except.throwError $ ParserException "Negative parser state"
    else do
        State.put $ ParserState (i - 1)
        c DummyToken
