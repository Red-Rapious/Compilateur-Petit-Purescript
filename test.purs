module Main where

import Prelude
import Effect
import Effect.Console

-- triangle de Pascal modulo 7

data List a = Nil | Cons a (List a)

printaux:: String -> List Int -> String
printaux acc Nil        = acc
printaux acc (Cons 0 l) = printaux (acc <> ".") l
printaux acc (Cons _ l) = printaux (acc <> "*") l

instance Show (List Int) where
  show l = printaux "" l

next:: Int -> List Int -> List Int
next p Nil        = Cons 1 Nil
next p (Cons x r) = Cons (mod (p+x) 7) (next x r)

pascal:: List Int -> Int -> Int -> Effect Unit
pascal r i n =
  if i < n then do log (show r)
                   pascal (next 0 r) (i+1) n
           else pure unit

main :: Effect Unit
main = pascal Nil 0 42



