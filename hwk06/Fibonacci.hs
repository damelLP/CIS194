{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
-- exercise 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- exercise 2
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- exercise 3
data Stream a = Stream a ( Stream a )

streamToList :: Stream a -> [a]
streamToList (Stream a as) = a : streamToList as

instance Show a => Show (Stream a) where 
  show stream = show $ take 20 $ streamToList stream

-- exercise 4
streamRepeat :: a -> Stream a
streamRepeat a = Stream a $ streamRepeat a

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream a as) = Stream (f a) $ streamMap f as

streamFromSeed :: (a->a) -> a -> Stream a
streamFromSeed f a = Stream a $ streamFromSeed f (f a)

-- exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a 
interleaveStreams (Stream a as) (Stream b bs) = Stream a $ Stream b $ interleaveStreams as bs

getPower :: Integer -> Integer 
getPower 0 = 0
getPower a
  | even a = 1 + getPower (a `div` 2)
  | otherwise = 0

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap getPower (streamFromSeed (+2) 2))

-- exercise 6
x :: Stream Integer
x = Stream 0 (Stream 1 (streamRepeat 0))

streamZipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
streamZipWith f (Stream a as) (Stream b bs) = Stream (f a b) (streamZipWith f as bs) 

instance Num (Stream Integer) where
  fromInteger n = Stream n (streamRepeat 0) 
  negate (Stream x xs) = Stream ( -1*x ) (negate xs)
  (+) = streamZipWith (+)
  (Stream a as) * (Stream b bs) = Stream (a*b) $ streamMap (*a) bs + as * Stream b bs

instance Fractional (Stream Integer) where 
  (Stream a as) / (Stream b bs) = Stream (a `div` b) $ streamMap (`div` b) (as - (bs * (Stream a as / Stream b bs)))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-- exercise 7

instance Num [[Integer]] where
  (*) [[a , b], [c, d]] [[a', b'], [c',d']] = [[a * a' + b * c', a * b' + b * d'], [c * a' + d * c', c * b' + d * d']]

f0 :: [[Integer]]
f0 = [[1,1],[1,0]]

fib4 :: Integer -> Integer
fib4 1 = 0
fib4 n = x where [[_,_],[x,_]] = f0^(n-1)
