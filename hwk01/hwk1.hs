-- exercise 1
toDigits :: Integer -> [Integer]
toDigits x 
  | x <=0 = []
  | otherwise = toDigits (x `div` 10) ++ [mod x 10] 

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther as = double (init as) ++ [last as]

double :: [Integer] -> [Integer]
double [] = []
double as = doubleEveryOther (init as) ++ [2 * last as]


-- exercise 3
sumDigits :: [Integer] -> Integer
sumDigits l = sum $ concatMap toDigits l

-- exercise 4
validate :: Integer -> Bool
validate num = mod a 10 == 0  
    where a = sumDigits . doubleEveryOther $ toDigits num

-- exercise 5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c 
    | n > 0 = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a
    | otherwise = []

-- exercise 6
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n a b c d
    | n > 3 = hanoi4 (n-3) a d b c ++ hanoi 2 a c b ++ [(a, b)] ++ hanoi 2 c b a ++ hanoi4 (n-3) d b a c
    | n == 3 = [(a,d), (a, c), (a,b), (c,b), (d,b)]
    | otherwise = hanoi n a b c

