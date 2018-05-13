{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import StackVM
import Calc
import Parser

instance Expr Program where 
  lit a = [PushI a]
  add a b = a ++ b ++ [Add]
  mul a b = a ++ b ++ [Mul]

compile :: String -> Maybe Program 
compile = parseExp lit add mul 

runExp :: String -> Either String StackVal
runExp str = case compile str of 
  (Just program) -> stackVM program
  Nothing -> Left "Compilation errors"
