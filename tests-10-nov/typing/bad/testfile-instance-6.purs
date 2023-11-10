module Main where
import Prelude
import Effect
import Effect.Console


class A a where
  foo:: a -> String
class B b where
  bar:: b -> String
instance A a => B Int where
  bar x = foo x
main :: Effect Unit
main = log "ok"
