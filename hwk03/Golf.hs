module Golf where

import Data.List


-- Hopscotch
skips :: [a] -> [[a]]
skips [] = []
skips (a:as) = (a:as) : skips as

-- Local maxima
localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:xs) 
  | b > a && b > c = b : localMaxima (c:xs)
  | otherwise = localMaxima (b:c:xs)
localMaxima _ = []

-- Histogram
histogram :: [Integer] -> String
histogram ls = let
  a = freqMap [0..9] $ (group . sort) ls 
  t = maximum a
  in createHistogram t a

-- returns star or not
starStr :: Integer -> Integer -> Char
starStr a b | b - a >= 0 = '*' | otherwise = ' '
  
-- creates histogram given frequencies
createHistogram :: Integer -> [Integer] -> String
createHistogram 0 _ = "==========\n0123456789"
createHistogram n ls = map (starStr n) ls ++ "\n" ++ createHistogram (n-1) ls

-- computes freq for each num
freqMap :: [Integer] -> [[Integer]] -> [Integer]
freqMap x [] = replicate (length x) 0
freqMap (a:as) (b:bs)
  | a == x = toInteger (length b ): freqMap as bs 
  | otherwise = 0 : freqMap as (b:bs)
  where (x:_) = b
freqMap _ _ = []

