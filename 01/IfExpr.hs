module IfExpr where

printSmallNumber num = 
  if num < 10
  then print num
  else print "the number is too big!"

printSmallNumber' num = 
  let msg = if num < 10
        then show num
        else "the number is too big!"
  in print msg

-- nesting if expressions gets tedious
sizeNumber num = 
  if num < 3
  then "that's a small number"
  else
    if num < 10
    then "that's a medium sized number"
    else "that's a big number"

-- there's a guard clause which is similar to rust's match expression
guardSize num
  | num < 3 = "that's a small number" 
  | num < 10 = "that's a medium number"
  | num < 100 = "that's a pretty big number"
  | num < 1000 = "wow, that's a giant number"
  | otherwise = "that's an unfathomably big number" -- otherwise is defined by the std and 
                                                    -- is basically just 'true'

-- let bindings are local to the specific branch
-- where bindings are available to the whole function
guardSize' num
  | num > 0 =
      let size = "positive"
      in exclaim size
  | num < 3 = exclaim "small"
  | num < 100 = exclaim "medium"
  | otherwise = exclaim "large"
  where
    exclaim message = "that's a " <> message <> " number!"

main = printSmallNumber 3
