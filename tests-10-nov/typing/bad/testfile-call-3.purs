module Main where
import Prelude
import Effect
import Effect.Console


f:: forall a. a -> a -> String
f x y = "yes"
main :: Effect Unit
main = log (f 1 true)
