module Main where

import Log

main :: IO ()
main = print $ check str l
  where
    str = "E 123  12323 aslkdjlas aslkdj"
    l = [(=="E"), isInt, isInt]
