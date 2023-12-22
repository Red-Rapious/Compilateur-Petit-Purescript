module Main where
import Prelude
import Effect
import Effect.Console


foo:: Boolean -> String
foo false = "world"
main :: Effect Unit
main = log ""
