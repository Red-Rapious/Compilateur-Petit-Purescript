module Main where
import Prelude
import Effect
import Effect.Console

class C where
  foo:: Int -> String
  bar:: Boolean -> String

instance C where
  foo 0 = "a"
  foo _ = "b"
  bar true = "c"
  bar false = "d"

main :: Effect Unit
main = log "ok"
