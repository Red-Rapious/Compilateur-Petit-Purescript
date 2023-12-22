module Main where

import Prelude
import Effect
import Effect.Console

-- tableaux moins naïfs (merci Okasaki)

data Pair a b = P a b
data List a = Nil | Zero (List (Pair a a)) | One a (List (Pair a a))

cons:: forall a. a -> List a -> List a
cons x Nil       = One x Nil
cons x (Zero l)  = One x l
cons x (One y l) = Zero (cons (P x y) l)

create:: Int -> List Int
create 0 = Nil
create n = cons 0 (create (n-1))

get:: forall a. a -> List a -> Int -> a
get d Nil       _ = d
get d (Zero l)  n = case get (P d d) l (n/2) of
                    P x0 x1 -> if mod n 2 == 0 then x0 else x1
get d (One x l) n = if n == 0 then x else get d (Zero l) (n-1)

set:: forall a. List a -> Int -> a -> List a
set Nil       _ _ = Nil
set (Zero l)  n v = case get (P v v) l (n/2) of
                    P x0 x1 -> Zero (set l (n/2)
                                      (if mod n 2 == 0 then P v x1 else P x0 v))
set (One x l) n v = if n == 0 then One v l
                    else cons x (set (Zero l) (n-1) v)

len:: forall a. List a -> Int
len Nil       = 0
len (Zero l)  = 2 * len l
len (One _ l) = 2 * len l + 1


data Answer = No | Yes (List Int)

to_string:: List Int -> Int -> Int -> Int -> String
to_string sol n k i =
  if k == n then "" else
  if i == n then "\n" <> to_string sol n (k+1) 0 else
  (if i == get 0 sol k then "Q" else ".") <> to_string sol n k (i+1)

instance Show Answer where
  show No      = "no"
  show (Yes l) = to_string l (len l) 0 0

abs:: Int -> Int
abs x = if x < 0 then -x else x

check:: Int -> List Int -> Int -> Int -> Boolean
check n sol k i =
  i == k || let d = get 0 sol i - get 0 sol k in
            d /= 0 && abs d /= k-i && check n sol k (i+1)

-- entrée : 0 <= k <= n et une solution partielle dans sol[0..k[
solve:: Int -> List Int -> Int -> Int -> Answer
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




