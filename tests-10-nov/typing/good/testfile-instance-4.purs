module Main where
import Prelude
import Effect
import Effect.Console


class A a where
  foo:: a -> String
class B b where
  bar:: b -> String
instance A a => B a where
  bar x = foo x
instance A Boolean where
  foo _ = "hello"

main :: Effect Unit
main = log (bar true)
