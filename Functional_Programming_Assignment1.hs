
type Var = String

data Term =
    Variable Var
  | Lambda   Var  Term
  | Apply    Term Term
  --deriving Show

--main :: IO()
--main = print ("Input below")
--main = do
  --print(numeral 2)
  --print([variables !! i | i <- [0,1,25,26,27,100,3039]])
  --print(filterVariables ["y","z","a1","a2"] ["y","a1","a3"])
  --print(fresh ["a","b","x"])
  --print(used example)
  --print(rename "b" "z" example)
  --print(substitute "b" (numeral 0) example)
  --print(Apply example (numeral 1))
  --print(normalize (Apply (numeral 2) (numeral 2)))
  --print(normal (Apply (numeral 2) (numeral 2)))

instance Show Term where
  show = pretty

example :: Term
example = Lambda "a" (Lambda "x" (Apply (Apply (Lambda "y" (Variable "a")) (Variable "x")) (Variable "b")))

pretty :: Term -> String
pretty = f 0
    where
      f i (Variable x) = x
      f i (Lambda x m) = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\" ++ x ++ ". " ++ f 0 m 
      f i (Apply  n m) = if i == 2 then "(" ++ s ++ ")" else s where s = f 1 n ++ " " ++ f 2 m

------------------------- Assignment 1

x :: Term
x = Variable "x"

f :: Term
f = Variable "f"

applyf :: Int -> Term
applyf n
    | (n > 0)   = Apply (f) (applyf(n-1))
    | otherwise = x

numeral :: Int -> Term
numeral n = Lambda "f" (Lambda "x" (applyf n))

succe :: Term
succe = Lambda "n" (Lambda "f" (Apply (Apply (Apply (Lambda "x" (f)) (Variable "n")) (f)) (x)))

plus :: Term
plus = Lambda "m" (Lambda "n" (Lambda "f" (Lambda "x" (Apply (Variable "m") (Apply (f) (Apply (Variable "n") (Apply(f) (x))))))))

-------------------------

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | (x == y)  = x : merge xs ys
    | (x <= y)  = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

------------------------- Assignment 2

variables :: [Var]
variables = [vs:[] | vs <- ['a'..'z']]
         ++ [vs:show xs | xs <- [1..], vs <- ['a'..'z']]

filterVariables :: [Var] -> [Var] -> [Var]
filterVariables xs ys = filter (\x -> x `notElem` ys) xs

fresh :: [Var] -> Var
fresh vs = (filterVariables variables vs) !! 0 

used :: Term -> [Var]
used (Variable x) = [x]
used (Lambda z n) = merge [z] (used n)
used (Apply  n m) = merge (used n) (used m)

------------------------- Assignment 3

rename :: Var -> Var -> Term -> Term
rename x y (Variable z)     
    | (z == x)  = Variable y
    | otherwise = Variable z
rename x y (Lambda z n)
    | (z == x)  = Lambda z n
    | otherwise = Lambda z a
    where a = rename x y n
rename x y (Apply n m) = Apply t1 t2
    where t1 = rename x y n
          t2 = rename x y m

substitute :: Var -> Term -> Term -> Term
substitute x y (Variable z)
    | (x == z)  = y
    | otherwise = Variable z
substitute x y (Lambda z n)
    | (x == z)  = Lambda z (substitute x y n)
    | otherwise = Lambda a b
    where
      a = fresh (used y ++ used n ++ [x])
      b = substitute x y (rename z a n ) 
substitute x y (Apply n m) = Apply (substitute x y n) (substitute x y m)

------------------------- Assignment 4

beta :: Term -> [Term]
beta (Variable z) = []
beta (Lambda z n) = [Lambda z a | a <- beta n]
beta (Apply (Lambda x n) m) = (substitute x m n) : [Apply (Lambda x a) m | a <- beta n]
                                                ++ [Apply (Lambda x n) a | a <- beta m]
beta (Apply n m) = [Apply a m | a <- beta n]
                ++ [Apply n a | a <- beta m]

normalize :: Term -> [Term]
normalize x
    | (length y == 0) = x : []
    | otherwise       = x : normalize (head y)
    where
      y = (beta x)

normal :: Term -> Term
normal x
    | (length y == 0) = x
    | otherwise       = normal (y !! 0)
    where
      y = (beta x)

------------------------- 
a_beta :: Term -> [Term]
a_beta (Variable z) = []
a_beta (Lambda z n) = [Lambda z a | a <- a_beta n]
a_beta (Apply (Lambda x n) m) = (substitute x m n) : [Apply (Lambda x a) m | a <- a_beta n]
                                                  ++ [Apply (Lambda x n) a | a <- a_beta m]
a_beta (Apply n m) = [Apply a m | a <- a_beta n]
                  ++ [Apply n a | a <- a_beta m]

a_normalize :: Term -> [Term]
a_normalize x
    | (length y == 0) = x : []
    | otherwise       = x : a_normalize (y !! (length y - 1))
    where
      y = (a_beta x)

a_normal :: Term -> Term
a_normal x
    | (length y == 0) = x
    | otherwise       = a_normal (y !! 0)
    where
      y = (a_beta x)

-------------------------

example1 :: Term
example1 = Apply (numeral 2) (numeral 2)

example2 :: Term
example2 = Apply (example1) (succe)

--example1Length :: Int
--example1Length = length (normalize example1)

--example1a_Length :: Int
--example1a_Length = length (a_normalize example1)

--example2Length :: Int
--example2Length = length (normalize example2)

--example2a_Length :: Int
--example2a_Length = length (a_normalize example2)