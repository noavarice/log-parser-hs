module Main where

import Log

main :: IO ()
main = (print . leftTrim) "   haaa"
