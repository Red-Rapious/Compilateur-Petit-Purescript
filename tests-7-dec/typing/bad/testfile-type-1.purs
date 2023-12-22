module Main where
import Prelude
import Effect
import Effect.Console

f:: Foo -> Foo
f x = x

main :: Effect Unit
main = log "hello"
