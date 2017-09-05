{-# LANGUAGE OverloadedStrings #-}
module TralaLexerInternal where

import Data.Void
import Data.Char
import Data.Int
import Data.Foldable

import Conduit
import TralaParsingCommon
import TralaAlexWrapper
import Control.Monad.Fix
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import TralaLexer
import System.IO
import Data.Text (Text)
import qualified Data.Text as T

type LexerState m = StateT AlexInput m
data TokenInfo = TokenInfo {
  getToken :: Token Text
  , getTokenInfoPosn :: AlexPosn
  , getString :: Text
  } deriving (Eq, Show)


tralaInputs :: Monad m =>
                 Conduit Text (LexerMonad m) AlexInput

tralaInputs = mapMC (\s -> do
                       (posn, prevChar, _, _) <- lift get
                       return (posn, prevChar, [], s)
                    )

data LexerException =
  LexerException AlexPosn Text
  deriving (Eq, Show)

class HasPosn c where
  getPosn :: c -> AlexPosn

instance HasPosn TokenInfo where
  getPosn = getTokenInfoPosn

instance HasPosn LexerException where
  getPosn (LexerException posn _) = posn

type LexerExcept m = ExceptT LexerException m
type LexerMonad m = LexerExcept (LexerState m)

tralaTokens :: Monad m => Conduit AlexInput (LexerExcept m) TokenInfo
tralaTokens = do
  maybeInput <- await
  case maybeInput of
    Just input@(posn, _, _, inputString) ->
      case alexScan input 0 of
        AlexEOF -> tralaTokens
        AlexError remainingInput -> do
          leftover remainingInput
          (lift . throwE) (LexerException posn "Lexer error")
        AlexSkip remainingInput len -> do
          leftover remainingInput
          tralaTokens
        AlexToken remainingInput tokLen tok -> do
          let tokenString = (T.take (fromIntegral tokLen) inputString)
          yield TokenInfo {
            getToken = tok tokenString
            , getTokenInfoPosn = posn
            , getString = tokenString
            }
          leftover remainingInput
          tralaTokens
    Nothing ->
      return ()


charsFromFile :: (Monad m, MonadResource m) => FilePath -> Conduit i m Char
charsFromFile fp = bracketP (openFile fp ReadMode) hClose (
  \h -> fix (\loop -> do
                eof <- (liftIO . hIsEOF) h
                if eof then
                  return ()
                else do
                  c <- (liftIO . hGetChar) h
                  yield c
                  loop
            )
  )


aggregate :: Monad m => Conduit Char m Text
aggregate = do
  nextChar <- peekC
  case nextChar of
    Nothing -> return ()
    Just c | isSpace c -> do
               takeAndPack isSpace
               aggregate
    Just c | otherwise -> do
               takeAndPack (not . isSpace)
               aggregate
    where
      takeAndPack p = takeWhileC p .| sinkList >>= (yield . T.pack)

textToInput :: Monad m => Conduit Text (LexerMonad m) AlexInput
textToInput = do
     i <- await
     case i of
       Just chunk -> do
         (posn, _, _, lastChunk) <- (lift . lift) get
         let newPos = foldl' alexMove posn (T.unpack chunk)
             newLastChar = T.last lastChunk
         (lift . lift . put) (newPos, newLastChar, [], chunk)
         yield (posn, newLastChar, [], chunk)
         textToInput
       Nothing -> return ()


alexInputsFromFile :: (Monad m, MonadResource m) => FilePath -> Conduit () (LexerMonad m) AlexInput
alexInputsFromFile fp =
   charsFromFile fp .| aggregate .| textToInput


alexConduitFromFile fp = alexInputsFromFile fp .| tralaTokens

runLexerConduit :: Monad m => ConduitM () Void (LexerMonad m) a -> AlexInput -> m (Either LexerException a)
runLexerConduit c s = evalStateT (runExceptT $ runConduit c) s

startAlexInput = (alexStartPos, '\n', [], "\n")

runLexerConduitFromStart c = runLexerConduit c startAlexInput
