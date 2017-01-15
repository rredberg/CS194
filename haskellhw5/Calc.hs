module Calc where
import ExprT
import Parser
import Data.Maybe

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = (eval x) + (eval y)
eval (Mul x y) = (eval x) * (eval y)

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr str
         | expr == Nothing = Nothing
         | otherwise = Just (eval x)
         where expr = parseExp Lit Add Mul str
               (Just x) = expr

reify :: ExprT -> ExprT
reify = id

-- Exercise 3
class Expr a where
      add :: a -> a -> a
      mul :: a -> a -> a
      lit :: Integer -> a

instance Expr ExprT where
    add x y = Add x y
    mul x y = Mul x y
    lit x = Lit x

instance Expr Integer where
    add x y = x + y
    mul x y = x * y
    lit x = x

instance Expr Bool where
    add x y = x || y
    mul x y = x && y
    lit x
       | x <= 0 = False
       | otherwise = True

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    add (MinMax x) (MinMax y) = MinMax (max x y)
    mul (MinMax x) (MinMax y) = MinMax (min x y)
    lit x = MinMax x

instance Expr Mod7 where
    add (Mod7 x) (Mod7 y) = Mod7 (mod (x+y) 7)
    mul (Mod7 x) (Mod7 y) = Mod7 (mod (x*y) 7)
    lit x = Mod7 x

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7