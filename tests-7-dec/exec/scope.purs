module Main where

import Prelude
import Effect
import Effect.Console

main :: Effect Unit
main =
  let x = "a" in
  do log x
     (let x = "b" in log x)
     log x
     (do log x
         (let x = "c" in log x))
     log x
     log (if 1 < 2 then let x = "d" in x else x)
     log (if 1 > 2 then let x = "d" in x else x)
