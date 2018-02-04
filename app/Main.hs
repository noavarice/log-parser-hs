module Main where

import Log

main :: IO ()
main = (print . getWord . leftTrim) "   haaa   asdad"
