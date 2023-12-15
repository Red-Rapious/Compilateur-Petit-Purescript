module Main where
import Prelude
import Effect
import Effect.Console


class C where
  foo:: Int -> String
  bar:: Boolean -> String

instance C where
  bar _ = "b"
  foo _ = "a"

main :: Effect Unit
main = log "ok"
