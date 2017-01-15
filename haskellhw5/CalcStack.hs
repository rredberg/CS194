{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where
import Data.Maybe
import Parser
import StackVM

-- ExprT has constructors Add. Mul so can't reuse them as constructors for Stack


-- Exercise 3
class Expr a where
      add :: a -> a -> a
      mul :: a -> a -> a
      lit :: Integer -> a


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

instance Expr Program where
    add x y = x ++ y ++ [Add]
    mul x y = x ++ y ++ [Mul]
    lit x = [PushI x]

compile :: String -> Maybe Program
compile = parseExp lit add mul

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7
