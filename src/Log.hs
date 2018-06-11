module Log ( validLogEntry
) where

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
