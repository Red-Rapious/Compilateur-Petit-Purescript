module Main where
import Prelude
import Effect
import Effect.Console


main = do log "a"
          do log "b"
             log "c"
