module Main where
import Prelude
import Effect
import Effect.Console


data T = C Int Int
f:: T -> Int
f (C x x) = x
main :: Effect Unit
main = log ""
