module Main where

import TralaParser
import TralaLexer

main :: IO ()
main = 
  fmap (show . scanAndParse) getContents >>= putStrLn
  where
      scanAndParse =
          either (error . getMessage) id .
              (evalState . runExceptT $ parse) .
                  (ParserState . alexScanTokens)
