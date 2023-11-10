module Main where

import Prelude
import Effect
import Effect.Console

main :: Effect Unit
main = do
  log (if false && true then "oups" else "phew...")
  log (if true || false then "phew..." else "oups")
  log (if 1 > 2 && 1/0 > 2 then "oups" else "phew...")
  log (if 1 < 2 || 1/0 > 2 then "phew..." else "oups")
