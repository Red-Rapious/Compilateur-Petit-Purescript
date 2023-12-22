module Main where

import Prelude
import Effect
import Effect.Console

fact:: Int -> Int
fact n = if n <= 1 then 1 else n * fact (n-1)

main :: Effect Unit
main = log (show (fact 10))

