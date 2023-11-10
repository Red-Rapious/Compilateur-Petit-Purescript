module Main where

import Prelude
import Effect
import Effect.Console

class A where
  foo:: Int -> String
class B where
  bar:: Int -> String
instance A => B where
  bar n = foo n
instance A where
  foo n = "hello"

main :: Effect Unit
main = log (bar 42)



