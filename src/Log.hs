module Log (
    parseMessage
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

data ParsingResult = ParsingResult String String

type Parser = String -> ParsingResult
type Validator = String -> Bool

data ChainLink = ChainLink Parser Validator

type Chain = [ChainLink]

skipDelimiter :: String -> String
skipDelimiter [] = []
skipDelimiter (x:xs) = if Data.Char.isSpace x then skipDelimiter xs else xs

parse :: String -> Chain -> LogMessage
parse [] _ = Unknown []
parse s [] = Unknown s
parse s ((ChainLink parser validator):xs) =
  if not isValid
    then Unknown s
    else parse withoutDelimiter xs
  where
    (ParsingResult parsed remained) = parser s
    isValid = validator parsed
    withoutDelimiter = skipDelimiter remained
