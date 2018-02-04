module Log ( parseMessage
           , leftTrim
           , getWord
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

isNotSpace = (not . Data.Char.isSpace)

skip :: [a] -> (a -> Bool) -> [a]
skip [] _ = []
skip list@(x:xs) f = if f x then list else skip xs f

leftTrim :: String -> String
leftTrim str = skip str isNotSpace

get :: [a] -> (a -> Bool) -> [a]
get [] _ = []
get list@(x:xs) f = if (not . f) x
                    then []
                    else x : get xs f

getWord :: String -> String
getWord str = get str isNotSpace
