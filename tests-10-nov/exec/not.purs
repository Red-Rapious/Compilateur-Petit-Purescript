module Main where

import Prelude
import Effect
import Effect.Console

main :: Effect Unit
main = do
  log (if not (1 < 2) then "a" else "b")
  log (if not (not (1 < 2)) then "a" else "b")
  log (show (not true))
  log (show (not false))
