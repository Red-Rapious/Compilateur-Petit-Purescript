module Main where

import Prelude
import Effect
import Effect.Console

data T = A Int | B String

foo:: T -> String
foo x = case x of A 0 -> "a"
                  A _ -> "b"
                  B s -> s

main :: Effect Unit
main = do log (foo (A 0))
          log (foo (A 1))
          log (foo (B "hello"))
