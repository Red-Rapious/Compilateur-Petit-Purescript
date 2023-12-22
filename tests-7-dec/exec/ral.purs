module Main where

import Prelude
import Effect
import Effect.Console

data Pair a b = P a b
data List a = Nil | Zero (List (Pair a a)) | One a (List (Pair a a))

cons:: forall a. a -> List a -> List a
cons x Nil       = One x Nil
cons x (Zero l)  = One x l
cons x (One y l) = Zero (cons (P x y) l)

create:: Int -> List Int
create 0 = Nil
create n = cons (n-1) (create (n-1))

make:: forall a. Int -> a -> List a
make 0 _ = Nil
make n v = let l = make (n/2) (P v v) in
           if mod n 2 == 0 then Zero l else One v l

instance (Show a, Show b) => Show (Pair a b) where
  show (P x y) = show x <> "," <> show y

instance Show a => Show (List a) where
  show Nil       = ""
  show (Zero l)  = "Z" <> show l
  show (One x l) = "O(" <> show x <> ")" <> show l

main :: Effect Unit
main = do log (show (create 4))
          log (show (create 10))
          log (show (create 42))
          log (show (make 4 1))
          log (show (make 10 2))
          log (show (make 42 3))

