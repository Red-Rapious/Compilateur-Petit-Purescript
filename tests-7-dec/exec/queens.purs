module Main where

import Prelude
import Effect
import Effect.Console

-- tableaux naïfs avec des listes

data List = Nil | Cons Int List

create:: Int -> List
create 0 = Nil
create n = Cons 0 (create (n-1))

get:: List -> Int -> Int
get Nil        _ = 42
get (Cons x l) n = if n == 0 then x else get l (n-1)

set:: List -> Int -> Int -> List
set Nil        _ _ = Nil
set (Cons x l) n v = if n == 0 then Cons v l else Cons x (set l (n-1) v)

len:: List -> Int
len Nil        = 0
len (Cons _ l) = 1 + len l

data Answer = No | Yes List

to_string:: List -> Int -> Int -> Int -> String
to_string sol n k i =
  if k == n then "" else
  if i == n then "\n" <> to_string sol n (k+1) 0 else
  (if i == get sol k then "Q" else ".") <> to_string sol n k (i+1)

instance Show Answer where
  show No      = "no"
  show (Yes l) = to_string l (len l) 0 0

abs:: Int -> Int
abs x = if x < 0 then -x else x

check:: Int -> List -> Int -> Int -> Boolean
check n sol k i =
  i == k || let d = get sol i - get sol k in
            d /= 0 && abs d /= k-i && check n sol k (i+1)

-- entrée : 0 <= k <= n et une solution partielle dans sol[0..k[
solve:: Int -> List -> Int -> Int -> Answer
solve n sol k v =
  if k == n then Yes sol else
  if v == n then No else
  let sol' = set sol k v in
  if check n sol' k 0 then
    case solve n sol' (k+1) 0 of
      Yes sol'' -> Yes sol''
      No        -> solve n sol k (v+1)
  else
    solve n sol k (v+1)

q:: Int -> Answer
q n = solve n (create n) 0 0

loop:: Int -> Effect Unit
loop 0 = pure unit
loop n = do loop (n-1)
            log (show n)
            log (show (q n))

main :: Effect Unit
main = loop 8




