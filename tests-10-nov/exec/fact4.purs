module Main where

import Prelude
import Effect
import Effect.Console

fact:: Int -> Int -> Int
fact acc 0 = acc
fact acc n = fact (acc * n) (n - 1)

main :: Effect Unit
main = log (show (fact 1 10))

