module Mapp where

doubleElems :: [Int] -> [Int]
doubleElems nums = 
  if null nums
  then []
  else
    let
      hd = head nums
      tl = tail nums
    in (2 * hd) : doubleElems tl

-- doubleElems' = foldr doubleElem []
--   where 
--     doubleElem num lst = (2 * num) : lst

doubleElems'' elems = foldr (applyElem (*2)) [] elems
  where
    applyElem f elem accumulator = f elem : accumulator

map' f = foldr (applyElem f) []
  where
    applyElem f elem accumulator = (f elem) : accumulator

map'' f xs =
  if null xs then []
  else f (head xs) : map'' f (tail xs)
