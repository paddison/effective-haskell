module FoodBudget where

checkGuestList guestList name = 
  name `elem` guestList

foodCosts = 
  [("Ren", 10.00)
  ,("George", 4.00)
  ,("Porter", 27.50)]

partyBudget isAttending = 
  foldr (+) 0 . map snd . filter (isAttending . fst)

partyBudget' isAttending willEat foodCost guests = 
  foldl (+) 0 $
  [ foodCost food
  | guest <- map fst guests
  , food  <- map snd guests
  , willEat guest food
  , isAttending guest
  ]


zip' as bs = 
  if null as || null bs then []
  else (head as, head bs) : zip' (tail as) (tail bs)
