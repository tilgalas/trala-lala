module TralaParsingCommon where

import Data.Text.Read
import Data.Text (Text)
import qualified Data.Text as T

data Token a = EOFToken |
             FloatLit Double |
             IntegerLit Int |
             Id a |
             EqualSign |
             DblEqualSign |
             DblColon |
             Operator a |
             LeftParen |
             RightParen |
             TrueLit |
             FalseLit |
             Underscore |
             Newline |
             Dot |
             Comma |
             StringLit a

    deriving (Eq, Show)


readAnything :: Reader a -> Text -> a

readAnything = ((either undefined (\(a, _) -> a)) .)

readIntegerLit :: Text -> Token Text
readIntegerLit = IntegerLit . (readAnything decimal)

readFloatLit :: Text -> Token Text
readFloatLit = FloatLit . (\t -> let fixedT =
                                       if T.head t == '.' then '0' `T.cons` t else t
                                 in
                                   readAnything double fixedT)

windowSearch :: Int -> (Text -> Bool) -> Text -> [Text]
windowSearch n p t
  | T.null t = []
  | otherwise = let (taken, rest) = T.splitAt n t in
                  if p taken then
                    taken : windowSearch n p rest
                  else
                    taken : windowSearch n p (T.tail t)

unescape2CharSeq :: Text -> Char
unescape2CharSeq t
  | (T.null . T.tail) t = T.head t
  | otherwise = let f = T.head t
                    s = (T.head . T.tail) t
                in
                  if f == '\\' then unescapeChar s else f

unescapeChar :: Char -> Char
unescapeChar c = case c of
                   '0' -> '\0'
                   'a' -> '\a'
                   'b' -> '\b'
                   'f' -> '\f'
                   'n' -> '\n'
                   'r' -> '\r'
                   't' -> '\t'
                   'v' -> '\v'
                   _ -> c

unescapeChars :: Text -> Text
unescapeChars t = T.pack $ unescape2CharSeq <$> windowSearch 2 (\t -> T.head t == '\\') t

readStringLit :: Text -> Token Text
readStringLit = StringLit . unescapeChars . T.init . T.tail
