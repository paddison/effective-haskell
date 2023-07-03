module Undefined where

addThree :: Int -> Int -> Int -> Int
addThree a = undefined

-- Depending on an implementation, addThree could return partiallu applied functions etc.
