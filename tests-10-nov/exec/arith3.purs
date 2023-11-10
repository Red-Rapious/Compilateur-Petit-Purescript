module Main where

import Prelude
import Effect
import Effect.Console

main :: Effect Unit
main = do
  log (show ((-10) / ( 3)))
  log (show (( 10) / (-3)))
  log (show ((-10) / (-3)))
  log (show (( 10) / ( 3)))
  log "Euclide contre la machine"


