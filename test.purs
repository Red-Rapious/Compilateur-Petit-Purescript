module Main where
import Prelude
import Effect
import Effect.Console


main = let x = "a"
           y = "b" in
    do
  log x
  log y