module Main where

import Prelude
import Effect
import Effect.Console

f:: String -> String -> String
f x y = x

main :: Effect Unit
main = do
  log (f "a" "b")
  log (f "b" "a")


