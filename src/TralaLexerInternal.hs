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

stringDelim = '"'

takeWhileNotEos :: Monad m => Conduit Char m Char
takeWhileNotEos = do
  takeWhileC (\c -> c /= '\\' && c /= stringDelim && c /= '\n')
  c <- peekC
  case c of
    Nothing -> return ()
    Just c | c == '\\' -> do
               takeC 2  -- peeked c and whatever was escaped
               takeWhileNotEos  -- resume
    Just c | otherwise -> return ()


aggregateStringLitContents :: Monad m => ConduitM Char o m Text
aggregateStringLitContents = do
  takeWhileNotEos .| sinkList >>= (return . T.pack)


aggregateStringLit :: Monad m => ConduitM Char o m Text
aggregateStringLit = do
  quot <- await
  case quot of
    Nothing -> return T.empty
    Just c -> do
      contents <- aggregateStringLitContents
      closeMaybe <- await  -- closing "
      return $ c `T.cons` contents `T.append` (maybe "" T.singleton closeMaybe)

aggregate :: Monad m => Conduit Char m Text
aggregate = do
  nextChar <- peekC
  case nextChar of
    Nothing -> return ()
    Just c | c == stringDelim -> do
               aggregateStringLit >>= yield
               aggregate
    Just c | isSpace c -> do
               takeAndPack isSpace >>= yield
               aggregate
    Just c | otherwise -> do
               takeAndPack (\c -> (not . isSpace) c && not (c == stringDelim)) >>= yield
               aggregate
    where
      takeAndPack p = takeWhileC p .| sinkList >>= (return . T.pack)

textToInput :: Monad m => Conduit Text (LexerMonad m) AlexInput
textToInput = mapMC (
  \chunk -> do
    (posn, _, _, lastChunk) <- lift get
    let newPos = foldl' alexMove posn (T.unpack chunk)
        newLastChar = T.last lastChunk
    (lift . put) (newPos, newLastChar, [], chunk)
    return (posn, newLastChar, [], chunk)
  )


alexInputsFromFile :: (Monad m, MonadResource m) => FilePath -> Conduit () (LexerMonad m) AlexInput
alexInputsFromFile fp =
   charsFromFile fp .| aggregate .| textToInput


alexConduitFromFile fp = alexInputsFromFile fp .| tralaTokens

runLexerConduit :: Monad m => ConduitM () Void (LexerMonad m) a -> AlexInput -> m (Either LexerException a)
runLexerConduit c s = evalStateT (runExceptT $ runConduit c) s

startAlexInput = (alexStartPos, '\n', [], "\n")

runLexerConduitFromStart c = runLexerConduit c startAlexInput
