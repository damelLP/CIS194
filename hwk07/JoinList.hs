{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module JoinList where

import Sized
import Scrabble
import Data.Monoid
import Debug.Trace
import Buffer

data JoinList m a = Empty 
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- exercise 1
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) jlA Empty = jlA
(+++) Empty jlA = jlA
(+++) jlA jlB = Append m jlA jlB where m = tag jlA <> tag jlB

tag :: Monoid m => JoinList m a -> m
tag Empty           = mempty
tag (Single m _)    = m
tag (Append m _ _ ) = m


-- exercise 2
jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

sizeOf :: Sized b => b -> Int
sizeOf = getSize . size

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty               = Nothing
indexJ i _ | i < 0           = Nothing
indexJ 0 (Single _ a)        = Just a
indexJ i (Single _ _ )       = Nothing 
indexJ i (Append n _ _) | i >= sizeOf n = Nothing 
indexJ i (Append n l1 l2) 
    | i < m = indexJ i l1
    | otherwise = indexJ (i - m) l2
    where m = sizeOf $ tag l1


dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i li | i <= 0 = li
dropJ i li | i >= (sizeOf . tag) li = Empty
dropJ i (Append j l1 l2) = left +++ right 
      where left = dropJ i l1
            right = dropJ (i - (sizeOf . tag) l1) l2

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i _ | i <= 0 = Empty
takeJ i li | i >= (sizeOf . tag) li = li
takeJ i (Append j l1 l2) = left +++ right 
      where left = takeJ i l1
            right = takeJ (i - (sizeOf . tag) l1) l2


-- exercise 3 
scoreLine :: String -> JoinList Score String
scoreLine [] = Empty 
scoreLine str = Single (scoreString str) str

-- exercise 4
jlToString :: JoinList m String -> String
jlToString Empty            = ""
jlToString (Single _ a)     = a 
jlToString (Append _ l1 l2) = jlToString l1 ++ jlToString l2

bufferFromString :: String -> JoinList (Score, Size) String 
bufferFromString str = foldr (+++) Empty $ map jlFromString $ lines str

jlFromString :: String -> JoinList (Score, Size) String
jlFromString x = Single (scoreString x, Size 1) x

replaceJ :: Int -> String -> JoinList (Score, Size) String -> JoinList (Score, Size) String
replaceJ i _ jl | i < 0 || i > sizeOf (tag jl) = jl 
replaceJ 0 str (Single _ _) = jlFromString str
replaceJ i str (Append n l1 l2) 
          | i < m = trace (show m) replaceJ i str l1 +++ l2
          | otherwise = l1 +++ replaceJ (i-m) str l2
          where m = sizeOf (tag l1)

          
instance Buffer (JoinList (Score, Size) String) where
  toString = jlToString
  fromString = bufferFromString
  line = indexJ
  replaceLine = replaceJ
  numLines = sizeOf . tag
  value Empty = 0
  value (Single (s, _) _) = getScore s
  value (Append (s, _) _ _) = getScore s


(!!?) :: [a] -> Int -> Maybe a
[] !!? _         = Nothing
_  !!? i | i < 0 = Nothing
(x:xs) !!? 0     = Just x
(x:xs) !!? i     = xs !!? (i-1)

test = Append (Product 210)
        (Append (Product 30)
          (Single (Product 5) 'y')
          (Append (Product 6)
            (Single (Product 2) 'e')
            (Single (Product 3) 'a')))
          (Single (Product 7) 'h')

jl = Append (Size 4)
        (Append (Size 3)
          (Single (Size 1) 'y')
          (Append (Size 2)
            (Single (Size 1) 'e')
            (Single (Size 1) 'a')))
          (Single (Size 1) 'h')

jl' = Append (Size 4)
        (Append (Size 2)
          (Single (Size 1) 'y')
          (Single (Size 1) 'e'))
        (Append (Size 2)
          (Single (Size 1) 'a')
          (Single (Size 1) 'h'))
