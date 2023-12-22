module Main where

import Prelude
import Effect
import Effect.Console

data T = A | B

foo:: T -> String
foo A = "hello"
foo B = "world"

main :: Effect Unit
main = do log (foo A)
          log (foo B)

