module Pattern where

handleNums l = 
 case l of 
  [] -> "An empty list"
  [x] | x == 0 -> "a list called: [0]"
      | x == 1 -> "a singular list of [1]"
      | even x -> "a singleton list containing an even number"
      | otherwise -> "the list contains " <> (show x)
  _list -> "the list has more than one element"

-- sometimes for incomplete patterns we can use the error function

partialFunction 0 = "I only work for 0"
-- partialFunction impossibleValue = error $
--   "I only work with 0 but i was called with " <> show impossibleValue
