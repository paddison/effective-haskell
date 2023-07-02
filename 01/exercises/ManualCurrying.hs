module ManualCurrying where

curry' f a b = f (a, b)

uncurry' f (a, b) = f a b

uncurriedAddition nums = 
  let
    a = fst nums
    b = snd nums
  in a + b
