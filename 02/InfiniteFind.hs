module InfiniteFind where

findFirst predicate = 
  foldr findHelper []
  where
    findHelper listElement maybeFound
      | predicate listElement = [listElement]
      | otherwise = maybeFound

foldr' func carryValue lst =
  if null lst
  then carryValue
  else func (head lst) $ foldr func carryValue (tail lst)


findFirst' predicate carryValue lst = 
  if null lst
  then carryValue
  else findHelper (head lst) $ findFirst' predicate carryValue (tail lst)
  where
    findHelper listElement maybeFound
      | predicate listElement = [listElement]
      | otherwise = maybeFound

-- refactoring findFirst'
findFirst'' predicate lst =
  if null lst
  then []
  else
    if predicate (head lst)
    then [head lst]
    else findFirst'' predicate (tail lst)


-- simplifying it more
findFirst''' predicate lst =
  if predicate (head lst)
  then [head lst]
  else findFirst''' predicate (tail lst)

