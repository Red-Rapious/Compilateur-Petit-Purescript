module Main where
import Prelude
import Effect
import Effect.Console


f:: forall a. a -> b
f x = x
main :: Effect Unit
main = log "hello"
