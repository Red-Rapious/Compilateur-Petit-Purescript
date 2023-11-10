module Main where
import Prelude
import Effect
import Effect.Console

log:: Int -> Int
log x = x
main :: Effect Unit
main = log "hello"
