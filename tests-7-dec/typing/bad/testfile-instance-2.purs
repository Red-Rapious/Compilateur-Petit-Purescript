module Main where
import Prelude
import Effect
import Effect.Console


instance D Int where
  foo _ = "hello"
main :: Effect Unit
main = log "ok"
