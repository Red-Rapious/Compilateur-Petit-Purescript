module Main where
import Prelude
import Effect
import Effect.Console


foo:: Int -> String
foo 1 = "world"
main :: Effect Unit
main = log ""
