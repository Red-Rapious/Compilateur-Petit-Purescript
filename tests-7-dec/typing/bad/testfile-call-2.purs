module Main where
import Prelude
import Effect
import Effect.Console


f:: forall a. a -> a
f x = x
main :: Effect Unit
main = log (f 1)
