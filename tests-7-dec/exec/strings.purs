module Main where

import Prelude
import Effect
import Effect.Console

main :: Effect Unit
main = do
  log "hello \    \world"
  log "hello \nworld"
  log "hello \\world"
  log "hello \"world\""
  log "hello \  
 \world"
  log "hello \  
  
 \world"
