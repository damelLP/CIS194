module JoinList where

import Sized
import Scrabble
import Data.Monoid
import Debug.Trace

data JoinList m a = Empty 
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- exercise 1
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
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
