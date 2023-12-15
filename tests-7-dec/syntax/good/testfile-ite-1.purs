module Main where
import Prelude
import Effect
import Effect.Console

main = if case 1 of 1 -> true
                    x -> false then do log "a"
                                       log "b"
                                  else do log "c"
                                          log "d"
