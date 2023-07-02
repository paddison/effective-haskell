module ReverseFolds where

reverseL lst = foldl helper [] lst
  where helper acc ele = ele : acc

reverseR lst = foldr helper [] lst
  where helper ele acc = acc <> [ele]
