module Concat where

concat' [] = []
concat' (ele:lst) = ele <> concat' lst


concatMap' f lst = foldr helper [] lst
  where 
    helper ele acc = (f ele) <> acc

concatMap'' f lst = foldl helper [] lst
  where 
    helper acc ele = acc <> (f ele)
