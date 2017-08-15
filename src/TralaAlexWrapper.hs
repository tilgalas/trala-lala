{-# LANGUAGE OverloadedStrings #-}

module TralaAlexWrapper where

import Data.Word (Word8)
import Data.Char (ord)
import qualified Data.Bits
import Data.Text(Text)
import qualified Data.Text as T


alex_tab_size = 8

data AlexPosn = AlexPn !Int  -- absolute character offset
                       !Int  -- line number
                       !Int  -- column number
              deriving (Eq, Show)

type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  [Word8],       -- rest of the bytes for the current char
                  Text)       -- current input string

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_, c, _, _) = c

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (p,c,(b:bs),s) = Just (b,(p,c,bs,s))
alexGetByte (_,_,[],"") = Nothing
alexGetByte (p,_,[],t)  = let p' = alexMove p c
                              c = T.head t
                              s = T.tail t
                              (b:bs) = utf8Encode c
                              in p' `seq`  Just (b, (p', c, bs, s))

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+alex_tab_size-1) `div` alex_tab_size)*alex_tab_size+1)
alexMove (AlexPn a l _) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)

utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
  where
    go oc
      | oc <= 0x7f       = [oc]
      | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                           , 0x80 + oc Data.Bits..&. 0x3f
                           ]
      | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                           , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                           , 0x80 + oc Data.Bits..&. 0x3f
                           ]
      | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                           , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                           , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                           , 0x80 + oc Data.Bits..&. 0x3f
                           ]
