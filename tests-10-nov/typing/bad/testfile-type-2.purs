module Main where
import Prelude
import Effect
import Effect.Console


f:: a -> a
f x = x
main :: Effect Unit
main = log "hello"
