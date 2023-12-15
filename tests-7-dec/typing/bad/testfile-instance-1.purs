module Main where
import Prelude
import Effect
import Effect.Console

class C a where
  foo:: a -> String
instance C T where
  foo _ = "hello"
main :: Effect Unit
main = log ""
