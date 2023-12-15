module Main where
import Prelude
import Effect
import Effect.Console

f:: Int -> String
f x = "hello"
main :: Effect Unit
main = log (f "")
