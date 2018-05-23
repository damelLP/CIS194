{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where
import Data.Char
import Data.Monoid

newtype Score = Score Int 
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int 
getScore (Score i) = i

class Scored a where 
  score :: a -> Score

instance Scored Score where 
  score = id
 
instance Scored a => Scored (a,b) where 
  score = score . fst

letterScore :: Char -> Score
letterScore rawC
  | c `elem` "eaionrtlsu" = Score 1
  | c `elem` "dg" = Score 2
  | c `elem` "bcmp" = Score 3
  | c `elem` "fhcwy" = Score 4
  | c `elem` "k" = Score 5
  | c `elem` "jx" = Score 8
  | c `elem` "qz" = Score 10
  | otherwise = Score 0
  where c = toLower rawC

scoreString :: String -> Score 
scoreString [] = Score 0
scoreString (a:as) = letterScore a + scoreString as

instance Monoid Score where 
  mempty = Score 0
  mappend = (+)

