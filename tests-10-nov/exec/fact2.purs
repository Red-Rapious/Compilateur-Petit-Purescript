module Main where

import Prelude
import Effect
import Effect.Console

fact:: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

main :: Effect Unit
main = log (show (fact 10))

