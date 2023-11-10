module Main where

import Prelude
import Effect
import Effect.Console

main :: Effect Unit
main = do
  log (show (1 - 1))
  log (show (0 + 1))
  log (show (1 + 2*0))
  log (show (5 / 2))
  log (show (1*1*3))
  log (show (75/5/3))
  log (show (7 + mod 1 2))
  log (show (2 + mod 143 12))
  log (show (20 - -1))
  log (show (- (0 - 34)))
