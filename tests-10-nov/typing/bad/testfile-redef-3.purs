module Main where
import Prelude
import Effect
import Effect.Console


f:: Int -> Int
f x = x
f y = y
main :: Effect Unit
main = log "hello"
