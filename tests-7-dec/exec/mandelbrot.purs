module Main where

import Prelude
import Effect
import Effect.Console

fp_add:: Int -> Int -> Int
fp_add x y = x + y

fp_sub:: Int -> Int -> Int
fp_sub x y = x - y

fp_mul:: Int -> Int -> Int
fp_mul x y = let t = x * y in (t + 8192 / 2) / 8192

fp_div:: Int -> Int -> Int
fp_div x y = let t = x * 8192 in (t + y / 2) / y

fp_of_int:: Int -> Int
fp_of_int x = x * 8192

iter :: Int -> Int -> Int -> Int -> Int -> Boolean
iter n a b xn yn =
  n == 100 ||
  let xn2 = fp_mul xn xn
      yn2 = fp_mul yn yn in
  fp_add xn2 yn2 <= fp_of_int 4 &&
  iter (n+1) a b (fp_add (fp_sub xn2 yn2) a)
                 (fp_add (fp_mul (fp_of_int 2) (fp_mul xn yn)) b)

inside :: Int -> Int -> Boolean
inside x y = iter 0 x y (fp_of_int 0) (fp_of_int 0)

line:: Int -> Int -> Int -> Int -> Int -> String -> String
line j steps y xmin deltax s =
  if j == steps then s else
    let x = fp_add xmin (fp_mul (fp_of_int j) deltax) in
    let s1 = if inside x y then s <> "0" else s <> "1" in
    line (j+1) steps y xmin deltax s1

row:: Int -> Int -> Int -> Int -> Int -> Int -> Effect Unit
row i steps xmin deltax ymin deltay =
  if i < steps then
    let y = fp_add ymin (fp_mul (fp_of_int i) deltay) in
    do log (line 0 (2*steps) y xmin deltax "")
       row (i+1) steps xmin deltax ymin deltay
  else
   pure unit

run:: Int -> Effect Unit
run steps =
  let
    xmin = fp_of_int (-2)
    xmax = fp_of_int 1
    deltax = fp_div (fp_sub xmax xmin) (fp_of_int (2*steps))
    ymin = fp_of_int (-1)
    ymax = fp_of_int 1
    deltay = fp_div (fp_sub ymax ymin) (fp_of_int steps)
  in
  row 0 steps xmin deltax ymin deltay

main :: Effect Unit
main = run 30

