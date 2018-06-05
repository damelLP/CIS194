module Party where

import Employee
import Data.Monoid
import Data.Tree
import Data.List

-- exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons employee (GL ls totalFun) = GL (employee:ls ) (totalFun + empFun employee)

mergeGL :: GuestList -> GuestList -> GuestList 
mergeGL (GL l1 f1) (GL l2 f2) = GL (l1++l2) (f1+f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun g1 g2 | g1 > g2 = g1 | otherwise = g2

instance Monoid GuestList where 
  mempty = GL [] 0
  mappend = mergeGL

-- exercise 2

treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold f b (Node a []) = f a [b]
treeFold f b (Node label children) = f label $ map (treeFold f b) children

emptyList = GL [] 0 :: GuestList

combineGLS :: Employee -> [GuestList] -> GuestList
combineGLS boss lists 
    | bf > ef = (GL (boss:ls) bf)
    | otherwise = (GL ls ef) 
    where bf = empFun boss 
          (GL ls ef) = foldr (<>) emptyList lists

-- exercise 3
mostFun :: [GuestList] -> GuestList
mostFun (a:as) = moreFun a $ mostFun as
mostFun [] = emptyList

-- concatenates a boss to a subordinate tree 
blCons :: Employee -> GuestList -> GuestList
blCons employee (GL ls totalFun) = GL (employee:ls) (empFun employee)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel employee [] = (glCons employee emptyList, emptyList)
nextLevel employee lists = (bestWith, bestWithout)
        where bestWith = moreFun (blCons employee withBosses) (glCons employee withoutBosses)
              bestWithout = moreFun withBosses withoutBosses
              (withBosses, withoutBosses) = ((foldr (<>) emptyList (map fst lists)), (foldr (<>) emptyList (map snd lists))) 

-- exercise 4
maxFun :: Tree Employee -> GuestList
maxFun node = moreFun a b 
          where (a,b) = treeFold nextLevel (emptyList, emptyList) node

-- exercise 5 

glString :: GuestList -> String 
glString (GL emps f) = unlines $ ("Total fun: " ++ show f) : sort employees where employees = map empName emps

main = do
    companyHierarchy <- readFile "company.txt"
    putStrLn $ glString $ maxFun $ read companyHierarchy
    
      
