module Main where

import Prelude
import Effect
import Effect.Console

f:: String -> String -> String -> String -> Effect Unit
f x y z t = if x /= "d" then do log x
                                f y z t x
                        else pure unit

main :: Effect Unit
main = f "a" "b" "c" "d"
