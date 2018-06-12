module Main where

import Log

main :: IO ()
main = readFile "app/sample.log" >>= (print . whatWentWrong . parse)
