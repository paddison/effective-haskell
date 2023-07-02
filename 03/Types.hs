module Types where
import Data.List
pi :: Float
pi = 3.14

-- several bindings with the same type
one, two :: Int
one = 1
two = 2

pi' = 3.14 :: Float -- single line type annotation

-- type annotations are not restricted to top level bindings
calculateTotalCost basePrice = 
  let 
    priceWithServiceFee :: Int
    priceWithServiceFee = basePrice + 1
    customaryTip = 7 :: Int
  in priceWithServiceFee + customaryTip

-- type annotations for functions
addOne :: Int -> Int
addOne n = n + 1

-- function types have parameters even when the parameter is not bound explicitly
addOne' :: Int -> Int
addOne' = (+ 1)

-- functions with multiple parameters
addThreeNumbers :: Int -> (Int -> (Int -> Int))
addThreeNumbers a b c = a + b + c
-- could also be written as
addThreeNumbers' = \a -> \b -> \c -> a + b + c

-- unfolding the example
--                :: x   -> (y   -> (z   -> Int))
addThreeNumbers'' :: Int -> (Int -> (Int -> Int))
addThreeNumbers'' x y z = 
  let 
    --   a   -> (g)
    f :: Int -> (Int -> (Int -> Int))
    f a =
      let 
        --   b   -> (h)
        g :: Int -> (Int -> Int)
        g b = 
          let 
            --   c   -> (a + b + c)
            h :: Int -> Int
            h c = a + b + c
          in h
        in g
      in f x y z

-- type annotations help improve readability for point-free and eta reduced functions
pointful :: [Int] -> Int -> Int
pointful xs n = foldr (+) 0 xs * n

-- the eta reduced version looks like this:
-- the parenthesis could be dropped
etaReduced :: [Int] -> (Int -> Int)     
etaReduced xs = (*) (foldr (+) 0 xs)

pointfree :: [Int] -> Int -> Int
pointfree = (*) . foldr (+) 0

-- A simple form of polymorphism:
-- 'a' is a 'type variable', they can also have longer names
identity :: a -> a
identity val = val

apply :: (a -> b) -> a -> b
apply f val = f val

incrementInt :: Int -> Int
incrementInt n = n + 1

incremented :: Int
incremented = apply incrementInt 1


-- using 'undefined' to check if the types of your function are correct
sumBiggest :: [[Int]] -> String
sumBiggest allNums = 
  let 
    getBiggests :: [Int] -> [Int]
    getBiggests nums = 
      let 
        maxNum :: Int
        maxNum = maximum nums
      in [n | n <- nums, n == maxNum]
    

    getSmallests :: [Int] -> [Int]
    getSmallests nums = 
      let 
        minNum :: Int
        minNum = minimum nums
      in [n | n <- nums, n == minNum]

    allBiggests :: [[Int]]
    allBiggests = map getBiggests allNums

    allSmallests :: [[Int]]
    allSmallests = map getSmallests allNums

    sizePairs :: [([Int], [Int])]
    sizePairs = zip allBiggests allSmallests

    differences :: ([Int], [Int]) -> Int
    differences (biggests, smallests) = sum biggests - sum smallests

    differences' :: [String]
    differences' = map (show . differences) sizePairs
  in Data.List.intercalate "," differences'


showBiggest =
  let biggestInfo = sumBiggest [[1,1,2,3,4,4],[1,2,5,5],[-1,-2,5,-10,5]]
  in print $ "sumBiggest says: " <> biggestInfo
