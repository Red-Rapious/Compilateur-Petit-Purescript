module Main where

import Prelude
import Effect
import Effect.Console

data T = A | B

foo:: T -> String
foo x = case x of A -> "hello"
                  B -> "world"

main :: Effect Unit
main = do log (foo A)
          log (foo B)

