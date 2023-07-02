module Degrees where

radsToDegrees :: Float -> Int
radsToDegrees radians = 
  let degrees = cycle' [0..359]
      converted = truncate $ (radians * 360) / (2 * pi)
  in degrees !! converted

cycle' inputList = 
  cycleHelper inputList
  where 
    cycleHelper [] = cycle' inputList
    cycleHelper (x:xs) = x : cycleHelper xs
