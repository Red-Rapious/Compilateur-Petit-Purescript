module Main where
import Prelude
import Effect
import Effect.Console

main :: Effect Unit
main = log (if true then "yes" else 1)
