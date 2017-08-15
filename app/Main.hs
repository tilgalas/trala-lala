module Main where

import TralaParser
import TralaLexer
import TralaLexerInternal
import qualified Data.Text.IO as TIO
import Conduit
import System.Environment

main :: IO ()
main = do
  filename <- head <$> getArgs
  runResult <- runResourceT $ runLexerConduitFromStart $ charsFromFile filename .| aggregate .|  textToInput .| tralaTokens .| mapM_C (liftIO . putStrLn . show)
  either (putStrLn . show) (return) runResult
  
