module Countdown where

countdown n =
  if n <= 0 then []
  else n : countdown (n - 1)

-- find prime factors of an integer
factors num =
  factors' num 2
  where
    factors' num fact
      | num == 1 = []
      | (num `rem` fact) == 0 = fact : factors' (num `div` fact) fact
      | otherwise = factors' num (fact + 1)


factors' num =
  let 
    factors'' num fact 
      | num == 1 = []
      | (num `rem` fact) == 0 = fact : factors'' (num `div` fact) fact
      | otherwise = factors'' num (fact + 1)
  in factors'' num 2


isBalanced s =
  0 == isBalanced' 0 s
  where
    isBalanced' count s
      | null s = count
      | head s == '(' = isBalanced' (count + 1) (tail s)
      | head s == ')' = isBalanced' (count - 1) (tail s)
      | otherwise = isBalanced' count (tail s)


reduce func carryValue lst =
  if null lst then carryValue
  else
    let intermediateValue = func carryValue (head lst)
    in reduce func intermediateValue (tail lst)

isBalanced' s = 0 == reduce myFunc 0 s
  where
    myFunc cv h
      | h == ')' = cv + 1
      | h == '(' = cv - 1
      | otherwise = cv
