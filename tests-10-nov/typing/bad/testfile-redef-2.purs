module Main where
import Prelude
import Effect
import Effect.Console


f:: Int -> Int
f x = x
f:: String -> String
f x = x
main :: Effect Unit
main = log "hello"
