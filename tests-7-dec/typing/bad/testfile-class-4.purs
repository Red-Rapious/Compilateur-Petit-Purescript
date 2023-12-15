module Main where
import Prelude
import Effect
import Effect.Console


foo::Int -> String
foo _ = "hello"
class C where
  foo:: Int -> String
main :: Effect Unit
main = log ""
