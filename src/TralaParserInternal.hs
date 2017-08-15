{-# LANGUAGE OverloadedStrings #-}
module TralaParserInternal where

import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import TralaParsingCommon
import Data.Text(Text)

type ParserMonad e s m = ExceptT e (StateT s m)

data ParserState = ParserState [Token Text]
data ParserException = ParserException {
    getMessage :: Text
}

type TralaParserMonad m = ParserMonad ParserException ParserState m

data EquationTerm = TokenTerm (Token Text) |
                    NestedTerm EquationTermTree
    deriving (Eq, Show)

type EquationTermTree = [EquationTerm]

data Equation = Equation {
    equationLhs :: EquationTermTree,
    equationRhs :: EquationTermTree
} deriving (Eq, Show)

parseError :: Monad m => (Token Text) -> TralaParserMonad m a
parseError _ = throwE $ ParserException "parse error"

lexer :: Monad m => ((Token Text) -> TralaParserMonad m a) -> TralaParserMonad m a
lexer c = do
    ParserState tokenList <- lift get
    case tokenList of
        [] -> c EOFToken
        tok:tokens -> do
            (lift . put) $ ParserState tokens
            c tok
