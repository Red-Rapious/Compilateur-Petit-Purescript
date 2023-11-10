module Main where

import Prelude
import Effect
import Effect.Console

f:: Unit -> Effect Unit
f _ = let x = "b" in log x

main :: Effect Unit
main = let x = "a" in
       do log x
          f unit
          log x



