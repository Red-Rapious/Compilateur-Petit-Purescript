module Main where

import Prelude
import Effect
import Effect.Console

main :: Effect Unit
main = do log (if 1 < 2 then "a" else "b")
          log (if 3 > 4 then "c" else "d")
          log (if true  then "e" else "f")
          log (if false then "g" else "h")
