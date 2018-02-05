module Log ( parseMessage
           , check
           , isInt
) where

import Data.Char

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
skip list@(x:xs) f = if f x then skip xs f else list

skipSpaces :: String -> String
skipSpaces str = skip str isSpace

skipWord :: String -> String
skipWord str = skip str isNotSpace

get :: [a] -> (a -> Bool) -> [a]
get [] _ = []
get list@(x:xs) f = if (not . f) x then [] else x : get xs f

getWord :: String -> String
getWord str = get str isNotSpace

check :: String -> [Validator] -> Bool
check [] _ = True
check _ [] = True
check str l = checkHelper (skipSpaces str) l

checkHelper :: String -> [Validator] -> Bool
checkHelper [] _ = True
checkHelper _ [] = True
checkHelper str (x:xs) = ((x . getWord) str) && (checkHelper ((skipSpaces . skipWord) str) xs)

isInt :: Validator
isInt [] = False
isInt [x] = isDigit x
isInt (x:xs) = (isDigit x) && (isInt xs)
