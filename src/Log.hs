module Log ( parse
) where

import Data.Char (ord)

data MessageType    = Info
                    | Warning
                    | Error Int
                    deriving (Show, Eq)

type TimeStamp  = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
                deriving (Show, Eq)

isDigit ch = (ch >= '0') && (ch <= '9')

isInt :: String -> Bool
isInt [] = False
isInt [x] = isDigit x
isInt (x:xs) = (isDigit x) && (isInt xs)

validLogEntry :: String -> Bool
validLogEntry [] = False
validLogEntry str = wordsCount >= 3 && isInt (w !! 1) && (otherMsg || errorMsg && wordsCount >= 4 && isInt (w !! 2))
  where
    w = words str
    wordsCount = length w
    msgType = head w
    errorMsg = msgType == "E"
    otherMsg = msgType == "I" || msgType == "W"

parseDigit :: Char -> Int
parseDigit x = ord x - ord '0'

nextSum :: Int -> Char -> Int
nextSum curSum ch = curSum * 10 + parseDigit ch

parseIntReq ::  String -> Int -> Int
parseIntReq [x] curSum = nextSum curSum x
parseIntReq (x:xs) curSum = parseIntReq xs (nextSum curSum x)

parseInt :: String -> Int
parseInt [x] = parseDigit x
parseInt str = parseIntReq str 0

getMessage :: String -> LogMessage
getMessage str = case head str of
  'I' -> LogMessage Info firstInt otherMsg
  'W' -> LogMessage Warning firstInt otherMsg
  _ -> LogMessage (Error firstInt) secondInt errorMsg
  where
    w = tail $ words str
    firstInt = parseInt $ head w
    secondInt = parseInt (w !! 1)
    withoutTypeAndTimestamp = tail w
    otherMsg = unwords withoutTypeAndTimestamp
    errorMsg = unwords $ tail withoutTypeAndTimestamp

parseMessage :: String -> LogMessage
parseMessage str =
  if validLogEntry str
    then getMessage str
    else Unknown str

parse :: String -> [LogMessage]
parse str = map parseMessage (lines str)
