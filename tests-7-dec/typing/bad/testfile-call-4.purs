module Main where
import Prelude
import Effect
import Effect.Console


f:: forall a b. a -> b
f x = "yes"
main :: Effect Unit
main = log (f 1)
