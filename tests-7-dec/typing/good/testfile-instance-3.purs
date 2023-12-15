module Main where
import Prelude
import Effect
import Effect.Console


class A where
  foo:: Int -> String
class B where
  bar:: Int -> String
instance A => B where
  bar n = foo n

main :: Effect Unit
main = log "ok"
