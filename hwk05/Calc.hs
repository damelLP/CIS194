module Calc where 
import ExprT
import Parser


-- exercise 1
eval :: ExprT -> Integer 
eval (Lit a) = a
eval (Mul a b) = eval a * eval b
eval (Add a b) = eval a + eval b

-- exercise 2
evalStr :: String -> Maybe Integer 
evalStr str = eval <$> parseExp Lit Add Mul str

-- exercise 3
class Expr a where
  lit :: Integer -> a 
  add :: a -> a -> a 
  mul :: a -> a -> a 

instance Expr ExprT where 
  lit = Lit
  add = Add 
  mul = Mul 

-- exercise 4
newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where 
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where 
  lit = (>0)
  add = (||)
  mul = (&&)

instance Expr MinMax where 
  lit = MinMax 
  add = max
  mul = min

instance Expr Mod7 where 
  lit a = Mod7 (mod a 7)
  add (Mod7 a) ( Mod7 b) = Mod7 $ (a+b) `mod` 7
  mul (Mod7 a) ( Mod7 b) = Mod7 $ (a*b) `mod` 7

-- Test
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

