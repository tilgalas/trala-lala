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
             Dot

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
