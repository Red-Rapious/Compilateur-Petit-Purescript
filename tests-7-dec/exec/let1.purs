module Main where

import Prelude
import Effect
import Effect.Console

main :: Effect Unit
main = do log (let x = "a" in x)
          log (let x = "a"
                   y = "b" in x)
          log (let x = "a"
                   y = "b" in y)
          log (let x = "a"
                   y = "b" in x <> y)
