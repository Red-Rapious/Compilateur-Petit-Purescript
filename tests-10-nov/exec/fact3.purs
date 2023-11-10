module Main where

import Prelude
import Effect
import Effect.Console

fact:: Int -> Int -> Int
fact acc n = if n == 0 then acc else fact (acc * n) (n - 1)

main :: Effect Unit
main = log (show (fact 1 10))

