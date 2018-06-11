module Main where

import Log

main :: IO ()
main = readFile "app/error.log" >>= (print . parse)
