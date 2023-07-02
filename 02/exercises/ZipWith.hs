{-# LANGUAGE ParallelListComp #-}
module ZipWith where

zipWith' f as bs = helper f as bs []
  where 
    helper f (a:as) (b:bs) lst 
      | null as || null bs = f a b : lst 
      | otherwise = f a b : helper f as bs lst

zipWith'' f as bs = [ f a b | a <- as | b <- bs ]

zipWith''' f as bs = foldl helper [] as 
  where
    helper acc a = a : acc       
