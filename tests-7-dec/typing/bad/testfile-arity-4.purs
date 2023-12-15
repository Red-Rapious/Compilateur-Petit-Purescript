module Main where
import Prelude
import Effect
import Effect.Console


f:: Int -> Int -> String
f x y = "a"
main :: Effect Unit
main = log (f 1)
