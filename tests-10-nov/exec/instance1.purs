module Main where

import Prelude
import Effect
import Effect.Console

class C where
  foo:: Int -> String

instance C where
  foo _ = "a"

main :: Effect Unit
main = log (foo 1)


