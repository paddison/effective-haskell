module UnderstandingFuncs where

swap' :: (a, b) -> (b, a)
swap' (a, b) = (b, a)

concat' :: [[a]] -> [a]
concat' lst = 
  if null lst then []
  else
    (head lst) <> (concat' $ tail lst)
