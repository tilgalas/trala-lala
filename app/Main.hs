module Main where

import TralaParser
import TralaLexer
import TralaLexerInternal
import Conduit
import System.Environment

main :: IO ()
main = do
  filename <- head <$> getArgs
  runResult <- runResourceT $ runLexerConduitFromStart $ alexInputsFromFile filename .| tralaTokens .| mapM_C (liftIO . putStrLn . show)
  either (putStrLn . show) (return) runResult
  
