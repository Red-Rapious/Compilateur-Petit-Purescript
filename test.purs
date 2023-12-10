module Main where

import Prelude
import Effect
import Effect.Console

<<<<<<< Updated upstream
main :: Effect Unit
main = do
  log (show (1 < 2))
  log (show (1 > 2))
  log (show (1 <= 2))
  log (show (1 >= 2))
  log (show (1 == 2))
  log (show (1 /= 2))
  log (show (true == false))
  log (show (true == true))
  log (show ("a" == "a"))
  log (show ("a" == "ab"))
  log (show ("a" /= "ab"))
  log (show (unit == unit))
  log (show (unit /= unit))


=======
class C a where
  foo:: a -> String
class C a where
  val:: a -> Int
main :: Effect Unit
main = log ""
>>>>>>> Stashed changes
