module Main where
import Prelude
import Effect
import Effect.Console


main :: Effect Unit
main = log (if 1 then "yes" else "no")
