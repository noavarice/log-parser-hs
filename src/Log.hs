module Log (
    parseMessage,
    leftTrim
) where

import qualified Data.Char

data MessageType    = Info
                    | Warning
                    | Error Int
                    deriving (Show, Eq)

type TimeStamp  = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
                deriving (Show, Eq)

parseMessage :: String -> LogMessage
parseMessage [] = Unknown []
parseMessage (x:xs) = LogMessage Info 13 "Text"

type Validator = String -> Bool

leftTrim :: String -> String
leftTrim [] = []
leftTrim str@(x:xs) = if (not . Data.Char.isSpace) x then str else leftTrim xs
