module Fibs where

incr n = n : incr (n + 1)

fib n
  | n == 0 = 0
 | n == 1 = 1
  | otherwise = (fib $ n - 1) + (fib $ n - 2)

fibs firstFib secondFib = 
  let nextFib = firstFib + secondFib
  in firstFib : fibs secondFib nextFib
