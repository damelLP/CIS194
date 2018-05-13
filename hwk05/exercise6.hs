{-# LANGUAGE FlexibleInstances #-}

import qualified Data.Map as M
import Calc

data VarExprT = VarLit Integer
            | VarAdd VarExprT VarExprT 
            | VarMul VarExprT VarExprT 
            | Var String
          deriving (Show, Eq)

class HasVars a where
  var :: String -> a

instance Expr VarExprT where 
  lit = VarLit 
  add = VarAdd 
  mul = VarMul 

instance HasVars VarExprT where 
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where 
  var  = M.lookup 

maybePlus :: Maybe Integer -> Maybe Integer -> Maybe Integer 
maybePlus (Just a) (Just b) = Just (a+b)
maybePlus _ _ = Nothing

maybeTimes :: Maybe Integer -> Maybe Integer -> Maybe Integer 
maybeTimes (Just a) (Just b) = Just (a*b)
maybeTimes _ _ = Nothing

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit a _ = Just a 
  add a b m = maybePlus (a m) (b m)
  mul a b m = maybePlus (a m) (b m)

withVars :: [(String, Integer)]
          -> (M.Map String Integer -> Maybe Integer) 
          -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

